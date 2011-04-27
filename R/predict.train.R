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
    
    if(object$modelType == "Regression" &&
       is.logical(object$control$predictionBounds) &&
       any(object$control$predictionBounds))
      {
        if(object$control$predictionBounds[1]) out <- ifelse(out < object$yLimit[1], object$yLimit[1], out)
        if(object$control$predictionBounds[2]) out <- ifelse(out > object$yLimit[2], object$yLimit[2], out)         
      }

    if(object$modelType == "Regression" &&
       is.numeric(object$control$predictionBounds) &&
       any(!is.na(object$control$predictionBounds)))
      {
        if(!is.na(object$control$predictionBounds[1])) out <- ifelse(out < object$control$predictionBounds[1], object$control$predictionBounds[1], out)
        if(!is.na(object$control$predictionBounds[2])) out <- ifelse(out > object$control$predictionBounds[2], object$control$predictionBounds[2], out)
      }

    out  
}



if(FALSE)
  {
    library(caret)
    library(mlbench)
    data(BostonHousing)


    lmFit <- train(medv ~ . + rm:lstat, "lm",
                   data = BostonHousing)

    lmFit2 <- train(medv ~ . + rm:lstat,
                    data = BostonHousing, 
                    "lm", trControl = trainControl(predictionBounds = rep(TRUE, 2)))
    lmFit5 <- train(medv ~ . + rm:lstat,
                    data = BostonHousing, 
                    "lm", trControl = trainControl(predictionBounds = c(TRUE, FALSE)))

    lmFit3 <- train(medv ~ . + rm:lstat,
                    data = BostonHousing, 
                    "lm", trControl = trainControl(predictionBounds = c(NA, 20)))

    lmFit4 <- train(medv ~ . + rm:lstat,
                    data = BostonHousing, 
                    "lm", trControl = trainControl(predictionBounds = c(10, 20)))

    summary(predict(lmFit, BostonHousing[, -14]))
    summary(predict(lmFit2, BostonHousing[, -14]))
    summary(predict(lmFit3, BostonHousing[, -14]))
    summary(predict(lmFit4, BostonHousing[, -14]))
    summary(predict(lmFit5, BostonHousing[, -14]))
    
  }
