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
    
    if(is.null(newdata))
      {
        if(!is.null(object$trainingData))
          {            
            newdata <- if(object$method == "pam") object$finalModel$xData else object$trainingData
            newdata$.outcome <-NULL
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



