preProcess <- function(x, ...)
  UseMethod("preProcess")

preProcess.default <- function(x, method = c("center", "scale"), thresh = 0.95, na.remove = TRUE, ...)
{

  if(all(c("pca", "ica") %in% method))
    {
      warning("fastICA automatically uncorrelates the data using PCA. method = 'pca' is not needed")
      method <- method[method != "pca"]
    }

  if(any(method == "pca") & !(any(method == "scale"))) method  <- c(method, "scale")
  if(any(method == "ica") & !(any(method == "center"))) method  <- c(method, "center")
  ## the row.norm option in fastICA states: "logical value indicating whether rows
  ## of the data matrix X should be standardized beforehand." Basically, this means that
  ## we would center *and* scale before the ICA step, so let's adjust the "scale" method too
  if(any(method == "ica"))
    {
      theDots <- list(...)
      row.norm <- if(is.null(list(...)$row.norm)) FALSE else list(...)$row.norm
      if(row.norm & !(any(method == "scale"))) method  <- c(method, "scale")
    }
  
  if(is.matrix(x))
    {
      if(!is.numeric(x)) stop("x must be numeric")
    }
  if(is.data.frame(x))
    {
      isFactor <- unlist(lapply(x, is.factor))
      isChar <- unlist(lapply(x, is.character))
      if(any(isFactor | isChar)) stop("all columns of x must be numeric")        
    }
  
  theCall <- match.call(expand.dots = TRUE)
  if(any(method  %in% c("center"))) centerValue <- apply(x, 2, mean, na.rm = na.remove) else centerValue <- NULL
  if(any(method %in% c("scale"))) scaleValue <- apply(x, 2, sd, na.rm = na.remove) else scaleValue <- NULL

  if(any(scaleValue == 0))
    {
      stop(
           paste(
                 "These variables have zero variances:",
                 paste(
                       names(scaleValue)[which(scaleValue == 0)],
                       collapse = ", ")))
    }
  
  if(any(method == "pca"))
    {
      tmp <- prcomp(x, scale = TRUE, retx = FALSE)
      cumVar <- cumsum(tmp$sdev^2/sum(tmp$sdev^2)) 
      numComp <- max(2, which.max(cumVar > thresh))
      rot <- tmp$rotation[,1:numComp]
    } else {
      rot <- NULL
      numComp <- NULL
    }

  if(any(method == "ica"))
    {
      set.seed(1)
      library(fastICA)
      x <- sweep(x, 2, centerValue, "-")
      if(!row.norm & any(method == "scale")) x <- sweep(x, 2, scaleValue, "/")      
      tmp <- fastICA(x, ...)
      ica <- list(
                  ## S = tmp$S, ## was used for debugging
                  row.norm = row.norm,
                  K = tmp$K,
                  W = tmp$W)
    } else {
      ica <- NULL
    }
  
  out <- list(call = theCall,
              dim = dim(x),
              mean = centerValue,
              std = scaleValue,
              rotation = rot,
              method = method,
              thresh = thresh,
              numComp = numComp,
              ica = ica,
              data = if(any(method == "knnImpute")) x else NULL)
  structure(out, class = "preProcess")
  
}

predict.preProcess <- function(object, newdata, ...)
{

  dataNames <- colnames(newdata)
  ## For centering and scaling, we can be flexible if a column in the
  ## original data set is not in newdata
  if(!is.null(object$mean))
    {
      if(!all(names(object$mean) %in% dataNames))
        {
          if(all(dataNames %in% names(object$mean)))
            {
              object$mean <- object$mean[names(object$mean) %in% dataNames]
              warning("newdata does not contain some variables")
            } else {
              vars <- dataNames[!(dataNames %in% names(object$mean))]
              stop(paste("The following variables were not pre-processed:",
                         paste(vars, collapse = ",")))
            }
        }
    }
  if(!is.null(object$std))
    {
      if(!all(names(object$std) %in% dataNames))
        {
          if(all(dataNames %in% names(object$std)))
            {
              object$std <- object$std[names(object$std) %in% dataNames]
            }
        }
    }
  if(!is.null(object$rotation))
    {
      if(!all(names(object$rotation) %in% dataNames))
        {
          stop("newdata does not contain some variables")
        }
    }


  oldClass <- class(newdata)
  if(any(object$method == "knnImpute"))
    {
      ## Look for missing values first
      library(pamr)
      nTrain <- nrow(object$data)
      ## The knn imputation is imlemented such that it can be done
      ## on a single data set
      browser()
      newdata <- rbind(object$data, newdata)
      ## This uses Euclidean distance so we should center and scale prior so
      ## that the distances are not overwhelmed by variables on larger scales.
      #newdata <- scale(newdata)
      #mns <- attr(newdata, "scaled:center")
      #sds <- attr(newdata, "scaled:scale")
      
      newdata <- pamr.knnimpute(list(x = t(object$data)))
      newdata <- t(newdata$x)
      newdata <- sweep(newdata, 2, sds, "*")
      newdata <- sweep(newdata, 2, mns, "+")
      newdata <- newdata[nTrain:nrow(newdata),, drop = FALSE]
    }
  
  if(any(object$method == "center")) newdata <- sweep(newdata, 2, object$mean, "-")
  if(any(object$method %in% c("scale"))) newdata <- sweep(newdata, 2, object$std, "/")
  if(any(object$method == "pca"))
    {
      newdata <-if(is.matrix(newdata)) newdata %*% object$rotation else as.matrix(newdata) %*% object$rotation
    }
  if(any(object$method == "ica"))
    {
      if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
      ##if(object$ica$row.norm) newdata <- apply(newdata, 1, function(u) u/sd(u))
      newdata <- newdata %*% object$ica$K %*% object$ica$W
      colnames(newdata) <- paste("ICA", 1:ncol(object$ica$W), sep = "")
    }

  
  if(any(oldClass == "data.frame")) newdata <- as.data.frame(newdata)

  if(any(object$method == "spatialsign")) newdata <- spatialSign(newdata)
  if(!(any(object$method %in% c("pca", "ica")))) colnames(newdata) <- dataNames
  newdata
}

print.preProcess <- function(x, ...)
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n")
  
  if(any(x$method == "pca"))
    {
      cat("PCA needed", x$numComp, "components to capture", round(x$thresh*100, 2),
          "percent of the variance\n")
    }
  if(any(x$method == "ica"))
    {
      cat("ICA used", ncol(x$ica$W), "components\n")
    }  
}

if(FALSE)
  {
    library(fastICA)
    library(caret)
    data(BloodBrain)

    data <- bbbDescr[, -nearZeroVar(bbbDescr)]

    set.seed(1)
    pp <- preProcess(data, method = c("center", "scale", "ica"), n.comp = 7, row.norm = TRUE)
    test2 <- predict(pp, data)

    ## You will need to uncomment a line in preProcess.default for this to work...
    max(head(pp$ica$S) - head(test2))

    head(pp$ica$S)
    head(test2)
  }
