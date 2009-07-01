predict.list <- function(object, ...)
  {

    out <- lapply(object, predict, ... = ...)
    if(!is.null(names(object))) names(out) <- names(object)
    out
  }

predict.train <- function(object, newdata = NULL, type = "raw", ...)
  {

    if(!(type %in% c("raw", "prob"))) stop("type must be either \"raw\" or \"prob\"")
    if(type == "prob")
      {
        if (any(!modelLookup(object$method)$probModel))
          stop("only classification models that produce probabilities are allowed")
      }

    if(!is.null(newdata))
      {
        if (inherits(object, "train.formula"))
          {
            newdata <- as.data.frame(newdata)
            rn <- row.names(newdata)
            Terms <- delete.response(object$terms)
            m <- model.frame(Terms, newdata, na.action = na.omit, 
                             xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses"))) 
              .checkMFClasses(cl, m)
            keep <- match(row.names(m), rn)
            newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
            xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
            if (xint > 0) 
              newdata <- newdata[, -xint, drop = FALSE]   
          }
      }
    else {
      if(!is.null(object$trainingData))
        {            
          newdata <- if(object$method == "pam") object$finalModel$xData else object$trainingData
          ##newdata$.outcome <-NULL
        } else stop("please specify data via newdata")
    }

    if(type == "prob")
      {
        out <- extractProb(
                           list(object),
                           unkX = newdata,
                           unkOnly = TRUE,
                           ...)
        obsLevels <- getClassLevels(object)
        out <- out[, obsLevels, drop = FALSE]
      } else {
        out <- extractPrediction(
                                 list(object),
                                 unkX = newdata,
                                 unkOnly = TRUE,
                                 ...)$pred
      }

    out  
  }



