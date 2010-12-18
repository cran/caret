"bag" <-
  function(x, ...)
  UseMethod("bag")


bagControl <- function(fit = NULL, predict = NULL, aggregate = NULL, downSample = FALSE)
  {

    list(fit = fit,
         predict = predict,
         aggregate = aggregate,
         downSample = downSample)
  }
  

"bag.default" <-
  function(x, y, B = 10, vars = NULL, bagControl = bagControl(),  ...)
{
  funcCall <- match.call(expand.dots = TRUE)

   if(!is.null(vars) && vars < 1) stop("vars must be an integer > 0")

  if(bagControl$downSample & is.numeric(y))
    {
      warning("down-sampling with regression... downSample changed to FALSE")
      bagControl$downSample <- FALSE
    }


  fitter <- function(index, x, y, ctrl, v, ...)
    {

      ## Add OOB summaries too
      subX <- x[index,, drop = FALSE]
      subY <- y[index]
      
      if(!is.null(v))
        {
          if(v < ncol(x)) v <- ncol(x)
          subVars <- sample(1:ncol(subX), ceiling(v))
          subX <- subX[, subVars, drop = FALSE]
        } else subVars <- NULL

      if(ctrl$downSample)
        {
          freaks <- table(subY)
          smallFreak <- min(freaks)
          splitUp <- split(seq(along = subY), subY)
          splitUp <- lapply(splitUp,
                            sample,
                            size = smallFreak)
          keepers <- unlist(splitUp)
          subX <- subX[keepers,,drop = FALSE]
          subY <- subY[keepers]
        }
      fit <- ctrl$fit(subX, subY, ...)

      list(fit = fit,
           vars = subVars)
    }
  
  
  btSamples <- createResample(y, times = B)
  btFits <- lapply(btSamples, fitter, x = x, y = y, ctrl = bagControl, v = vars, ...)

  structure(
            list(fits = btFits,
                 control = bagControl,
                 call = funcCall,
                 B = B,
                 vars = vars,
                 smallClass = min(table(y)),
                 dims = dim(x)),
            class = "bag")

}



"bag.formula" <-
  function (formula, data = NULL,..., subset, weights, na.action = na.omit) 
{
  funcCall <- match.call(expand.dots = TRUE)
  
  if (!inherits(formula, "formula")) 
    stop("method is only for formula objects")
  m <- match.call(expand = FALSE)  
  mIndex <- match(c("formula", "data", "subset", "weights", "na.action"), names(m), 0)
  m <- m[c(1, mIndex)]
  m$... <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  y <- model.response(m)
  w <- model.weights(m)
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  
  out <- bag.default(x, y, ...)
  out$call <- funcCall
  out
}

"predict.bag" <-
  function(object, newdata = NULL, ...)
{
 
  if(is.null(newdata)) stop("please provide a data set for prediction")

  predictor <- function(obj, x, ctrl)
    {
      if(!is.null(obj$vars)) x <- x[, obj$vars, drop = FALSE]
      pred <- ctrl$predict(obj$fit, x)
    }
  btPred <- lapply(object$fit, predictor, x = newdata, ctrl = object$control)
  object$control$aggregate(btPred, ...)
  
}

print.bag <- function (x, ...) 
{
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
  cat("\nB:", x$B,"\n")

  cat("Training data:", x$dims[2], "variables and", x$dims[1], "samples\n")
  cat(ifelse(is.null(x$vars),
             "All variables were used in each model",
             paste("Each model used", x$control$vars, "random",
                   ifelse(x$control$vars == 1, "variable", "variables"), "predictors")))
  cat('\n')
  if(x$control$downSample)
    {
      cat("Training data was down-sampled to balance the classes to",
          x$smallClass, "samples per class\n\n")
    }
             
  invisible(x)
}

"summary.bag" <-
  function(object, ...)
{

  if(object$control$oob)
    {
      oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1)))
    } else oobStat <- NULL
  out <- list(oobStat = oobStat, call = object$call)
  class(out) <- "summary.bag"
  out
}

"print.summary.bag" <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if(!is.null(x$oobStat))
    {
      cat("Out of bag statistics:\n\n")
      print(x$oobStat, digits = digits)
    }
  cat("\n")
}


ldaBag <- list(fit = function(x, y, ...)
               {
                 library(MASS)
                 lda(x, y, ...)
               },

               pred = function(object, x)
               {
                 predict(object, x)$posterior
               },
               aggregate = function(x, type = "class")
               {
                 ## The class probabilities come in as a list of matrices
                 ## For each class, we can pool them then average over them

                 pooled <- x[[1]] & NA
                 classes <- colnames(pooled)
                 for(i in 1:ncol(pooled))
                   {
                     tmp <- lapply(x, function(y, col) y[,col], col = i)
                     tmp <- do.call("rbind", tmp)
                     pooled[,i] <- apply(tmp, 2, median)
                   }
                 pooled <- apply(pooled, 1, function(x) x/sum(x))
                 if(type == "class")
                   {
                     out <- factor(classes[apply(pooled, 1, which.max)],
                                   levels = classes)
                   } else out <- pooled
                 out
               })


plsBag <- list(fit = function(x, y,  ...)
               {
                 library(pls)
                 plsda(x, y, ...)
               },

               pred = function(object, x)
               {
                 predict(object, x, type = "prob")[,,]
               },
               aggregate = function(x, type = "class")
               {
                
                 pooled <- x[[1]] & NA
                 classes <- colnames(pooled)
                 for(i in 1:ncol(pooled))
                   {
                     tmp <- lapply(x, function(y, col) y[,col], col = i)
                     tmp <- do.call("rbind", tmp)
                     pooled[,i] <- apply(tmp, 2, median)
                   }
                 if(type == "class")
                   {
                     out <- factor(classes[apply(pooled, 1, which.max)],
                                   levels = classes)
                   } else out <- pooled
                 out
               })



treeBag <- list(fit = function(x, y,  ...)
                {
                  library(party)
                  data <- as.data.frame(x)
                  data$y <- y
                  ctree(y~., data = data)
                },

                pred = function(object, x)
                {
                 
                  obsLevels <-  levels(object@data@get("response")[,1])
                  rawProbs <- treeresponse(object, x)
                  probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                  out <- data.frame(probMatrix)
                  colnames(out) <- obsLevels
                  rownames(out) <- NULL
                  out
                },
                aggregate = function(x, type = "class")
                {
                  
                  pooled <- x[[1]] & NA
                  classes <- colnames(pooled)
                  for(i in 1:ncol(pooled))
                    {
                      tmp <- lapply(x, function(y, col) y[,col], col = i)
                      tmp <- do.call("rbind", tmp)
                      pooled[,i] <- apply(tmp, 2, median)
                    }
                  if(type == "class")
                    {
                      out <- factor(classes[apply(pooled, 1, which.max)],
                                    levels = classes)
                    } else out <- pooled
                  out
                })


