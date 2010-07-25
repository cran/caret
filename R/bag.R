"bag" <-
  function(x, ...)
  UseMethod("bag")


bagControl <- function(fit = NULL, predict = NULL, aggregate = NULL)
  {

    list(fit = fit,
         predict = predict,
         aggregate = aggregate)
  }
  

"bag.default" <-
  function(x, y, B = 10, vars = NULL, bagControl = bagControl(),  ...)
{
  funcCall <- match.call(expand.dots = TRUE)

   if(!is.null(vars) && vars < 1) stop("vars must be an integer > 0")

  fitter <- function(index, x, y, ctrl, v, ...)
    {

      subX <- x[index,, drop = FALSE]
      if(!is.null(v))
        {
          if(v < ncol(x)) v <- ncol(x)
          subVars <- sample(1:ncol(subX), ceiling(v))
          subX <- subX[, subVars, drop = FALSE]
        } else subVars <- NULL
      subY <- y[index]    
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
  object$control$aggregate(btPred)
  
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
             

  invisible(x)
}

"summary.bag" <-
  function(object, ...)
{
  
  oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1)))


  out <- list(oobStat = oobStat, call = object$call)
  class(out) <- "summary.bag"
  out
}

"print.summary.bag" <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  cat("Out of bag statistics:\n\n")
  print(x$oobStat, digits = digits)
  cat("\n")
}



