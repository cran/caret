preProcess <- function(x, ...)
  UseMethod("preProcess")

preProcess.default <- function(x, method = c("center", "scale"),
                               thresh = 0.95, na.remove = TRUE, k = 5,
                               knnSummary = mean,
                               ...)
{

  if(all(c("pca", "ica") %in% method))
    {
      warning("fastICA automatically uncorrelates the data using PCA. method = 'pca' is not needed")
      method <- method[method != "pca"]
    }

  if(any(method == "spatialSign") & !(any(method == "scale"))) method  <- c(method, "scale")
  if(any(method == "spatialSign") & !(any(method == "center"))) method  <- c(method, "center")
  
  if(any(method == "pca") & !(any(method == "scale"))) method  <- c(method, "scale")
  if(any(method == "pca") & !(any(method == "center"))) method  <- c(method, "center")
  
  if(any(method == "ica") & !(any(method == "scale"))) method  <- c(method, "scale")
  if(any(method == "ica") & !(any(method == "center"))) method  <- c(method, "center")

  if(any(method == "knnImpute") & any(method == "bagImpute"))
    stop("please pick only one imputation method")
    
  method <- unique(method)
  
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
  if(any(method  %in% c("center", "knnImpute"))) centerValue <- apply(x, 2, mean, na.rm = na.remove) else centerValue <- NULL
  if(any(method %in% c("scale", "knnImpute"))) scaleValue <- apply(x, 2, sd, na.rm = na.remove) else scaleValue <- NULL

  if(any(scaleValue == 0))
    {
      warning(
              paste(
                    "These variables have zero variances:",
                    paste(
                          names(scaleValue)[which(scaleValue == 0)],
                          collapse = ", ")))
      scaleValue[which(scaleValue == 0)] <- 1
    }

  cols <- if(any(method == "knnImpute")) which(apply(x, 2, function(x) !any(is.na(x)))) else NULL

  if(any(method == "bagImpute"))
    {
      bagModels <- as.list(colnames(x))
      names(bagModels) <- colnames(x)
      bagModels <- lapply(bagModels,
                          bagImp,
                          x = x)    
    } else bagModels <- NULL
  
  x <- x[complete.cases(x),]
  
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
              k = k,
              knnSummary = knnSummary,
              bagImp = bagModels,
              cols = cols,
              data = if(any(method == "knnImpute")) scale(x[complete.cases(x),]) else NULL)
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
  cc <- complete.cases(newdata)
  if(any(object$method == "knnImpute") && any(!cc))
    {
      
      ## First, center and scale
      hasMiss <- newdata[!cc,,drop = FALSE]      
      hasMiss <- sweep(hasMiss, 2, object$mean, "-")
      hasMiss <- sweep(hasMiss, 2, object$std, "/")

      hasMiss <- apply(hasMiss,
                       1,
                       nnimp,
                       old = object$data,
                       cols = object$cols,
                       k = object$k,
                       foo = object$knnSummary)
      hasMiss <- t(hasMiss)

      ## Transform back
      hasMiss <- sweep(hasMiss, 2, object$std, "*")
      hasMiss <- sweep(hasMiss, 2, object$mean, "+")
      newdata[!cc,] <- hasMiss
    }
  
  if(any(object$method == "bagImpute") && any(!cc))
    {
      library(ipred)
      hasMiss <- newdata[!cc,,drop = FALSE]
      missingVars <- apply(hasMiss,
                           2,
                           function(x) any(is.na(x)))
      missingVars <- names(missingVars)[missingVars]
      for(i in seq(along = missingVars))
        {
          preds <- predict(object$bagImp[[missingVars[i]]]$model,
                           hasMiss[, !colnames(hasMiss) %in% missingVars[i]])
          
          hasMiss[is.na(hasMiss[,missingVars[i]]),
                  missingVars[i]] <- preds[is.na(hasMiss[,missingVars[i]])]
        }
      newdata[!cc,] <- hasMiss
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

  if(any(object$method == "spatialSign")) newdata <- spatialSign(newdata)
  if(!(any(object$method %in% c("pca", "ica")))) colnames(newdata) <- dataNames
  newdata
}

print.preProcess <- function(x, ...)
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n")

  pp <- x$method
  pp <- gsub("scale", "scaled", pp)
  pp <- gsub("center", "centered", pp)
  pp <- gsub("pca", "principal component signal extraction", pp)
  pp <- gsub("ica", "independent component signal extraction", pp)
  pp <- gsub("spatialSign", "spatial sign transformation", pp)
  pp <- gsub("knnImpute", paste(x$k, "nearest neighbor imputation"), pp)
  pp <- gsub("bagImpute", "bagged tree imputation", pp)  

  cat("Pre-processing:",
      paste(pp, collapse = ", "),
      "\n")
  
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


nnimp <- function(new, old, cols, k, foo)
  {
    library(RANN)
    nms <- names(new)
    cols2 <- which(!is.na(new))
    new <- matrix(new, ncol = length(new))
    colnames(new) <- nms
    cols <- sort(intersect(cols2, cols))
    nn <- nn2(old[, cols, drop = FALSE],
              new[, cols, drop = FALSE],
              k = k)
    tmp <- old[nn$nn.idx, -cols, drop = FALSE]
    ##TODO deal with cases where training set has missing data
    subs <- apply(tmp, 2, foo, na.rm = TRUE)
    new[, -cols] <- subs
    new
  }

bagImp <- function(var, x, B = 10)
  {
    library(ipred)
    ## The formula interface is much slower than the
    ## (y, X) interface, but the latter would have to
    ## do case-wise deletion of samples from the
    ## training set.
    mod <- bagging(as.formula(paste(var, "~.")),
                   data = x,
                   nbagg = B)
    list(var = var,
         model = mod)
  }


if(FALSE)
  {
    library(fastICA)
    library(caret)
    data(BloodBrain)

    data <- bbbDescr[, -nearZeroVar(bbbDescr)][, 1:20]
    data[1, 1:20] <- NA
    data[2:10, 4] <- NA

    trainData <- data

    set.seed(1)
    pp <- preProcess(data, method = c("center", "scale", "bagImpute", "ica"))
    test2 <- predict(pp, data)

    ## You will need to uncomment a line in preProcess.default for this to work...
    max(head(pp$ica$S) - head(test2))

    head(pp$ica$S)
    head(test2)



    library(caret)
    data(BloodBrain)

    data(BloodBrain)

    data <- bbbDescr[, -nearZeroVar(bbbDescr)]
    data2 <- data
    data2[1, 1:20] <- NA
    data2[2:10, 4] <- NA

    trainData <- data2[11:nrow(data2),]
    testData <- data2[1:10,]
    
    set.seed(1)
    pp1 <- preProcess(trainData, method = c("bagImpute"))
    pp2 <- preProcess(trainData, method = c("knnImpute"))

    data3 <- predict(pp1, testData)
    data4 <- predict(pp2, testData)
    splom(~cbind(data[1:10,"a_aro"], data3[, "a_aro"], data4[,"a_aro"]))
    
  }

