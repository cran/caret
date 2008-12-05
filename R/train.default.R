"train" <-
  function(x, ...){
    UseMethod("train")
  }

train.default <- function(x, y, 
                          method = "rf", 
                          ..., 
                          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                          maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                          trControl = trainControl(),
                          tuneGrid = NULL, 
                          tuneLength = 3)
{

  funcCall <- match.call(expand.dots = TRUE)
  
  modelType <- if(is.factor(y)) "Classification"  else "Regression"
  modelInfo <- modelLookup(method)
  
  if(modelType == "Classification")
    {     
      if(!any(modelInfo$forClass)) stop("wrong model type for classification")
      ## We should get and save the class labels to ensure that predictions are coerced      
      ## to factors that have the same levels as the original data. This is especially 
      ## important with multiclass systems where one or more classes have low sample sizes
      ## relative to the others
      classLevels <- levels(y)
      if(length(classLevels) > 2 & (method %in% c("gbm", "glmboost", "ada", "gamboost", "blackboost", "penalized", "glm")))
        stop("This model is only implimented for two class problems")
      if(metric %in% c("RMSE", "Rsquared")) 
        stop(paste("Metric", metric, "not applicable for classification models"))
    } else {
      if(!any(modelInfo$forReg)) stop("wrong model type for regression")
      if(metric %in% c("Accuracy", "Kappa")) 
        stop(paste("Metric", metric, "not applicable for regression models"))         
      classLevels <- NA
    }
  
  if(trControl$method == "oob" & !(method %in% c("rf", "treebag", "cforest", "bagEarth", "bagFDA")))
    stop("for oob error rates, model bust be one of: rf, cforest, bagEarth, bagFDA or treebag")
  
  if(is.null(trControl$index)) trControl$index <- switch(
                                                         tolower(trControl$method),
                                                         oob = NULL,
                                                         cv = createFolds(y, trControl$number, returnTrain = TRUE),
                                                         loocv = createFolds(y, length(y), returnTrain = TRUE),
                                                         boot = createResample(y, trControl$number),
                                                         test = createDataPartition(y, 1, trControl$p),
                                                         lgocv = createDataPartition(y, trControl$number, trControl$p))
  
  ## Combine the features and classes into one df, as needed by ipred.
  trainData <- as.data.frame(x)

  ## Check mode for some models
  if(!(method %in% c("rf", "rpart", "gbm", "treebag", "nb")))
    {
      isFactor <- lapply(trainData, is.factor)
      isCharacter <- lapply(trainData, is.character)
      if(any(unlist(isFactor))   | any(unlist(isCharacter)))  
        stop("All predictors must be numeric for this model") 
    }

  ## Add the outcome to the data passed into the functions
  trainData$.outcome <- y

  ## If no default training grid is specified, get one. We have to pass in the formula
  ## and data for some models (rpart, pam, etc - see manual fo rmore details)
  if(is.null(tuneGrid)) tuneGrid <- createGrid(method, tuneLength, trainData)

  ##------------------------------------------------------------------------------------------------------------------------------------------------------#

  ## For each tuning parameter combination, we will loop over them, fit models and generate predictions.
  ## We only save the predictions at this point, not the models (and in the case of method = "oob" we 
  ## only save the prediction summaries at this stage.
  
  ## trainInfo will hold teh infomration about how we should loop to train the model and what types
  ## of parameters are used. If method = "oob", we need to setip a container for the resamplng 
  ## summary statistics 
  
  trainInfo <- tuneScheme(method, tuneGrid, trControl$method == "oob")
  paramCols <- paste(".", trainInfo$model$parameter, sep = "")

  if(trainInfo$scheme == "oob")
    {
      ## For oob performance, we will make a container for the results
      ## In other cases, it will be created on the fly after the predictions are made
      if(modelType == "Regression")
        {
          performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(trainInfo$loop)[1]))   
          names(performance) <- c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")      
        } else {
          performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(trainInfo$loop)[1]))   
          names(performance) <- c("Accuracy", "Kappa", "AccuracySD", "KappaSD")
        }
      perfNames <- c("Accuracy", "Kappa")
    } else {
      testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                               obs = sample(y, min(10, length(y))))
      perfNames <- names(trControl$summaryFunction(testOutput,
                                                   classLevels,
                                                   method))
    }

  if(!(metric %in% perfNames))
    {
      oldMetric <- metric
      metric <- perfNames[1]
      warning(paste("The metric \"",
                    oldMetric,
                    "\" was not in ",
                    "the result set. ",
                    metric,
                    " will be used instead.",
                    sep = ""))
    }

  results <- NULL
  for(j in 1:nrow(trainInfo$loop))
    {

      if(trControl$verboseIter)
        {
          iterPrint(trainInfo, j)            
          flush.console() 
        }
      
      argList <- list(
                      data = trainData,
                      method = method,
                      tuneValue = trainInfo$loop[j,, drop = FALSE],
                      obsLevels = classLevels)
      argList <- append(argList, list(...))         
      
      
      switch(
             trainInfo$scheme,
             basic = 
             {
               thisIter <- byResampleBasic(
                                           trControl$index,
                                           argList,
                                           trainInfo$loop[j, trainInfo$constant,drop = FALSE]) 
               results <- rbind(results, thisIter)
             },
             seq = 
             {
               thisIter <- byResampleSeq(
                                         trControl$index,
                                         argList,
                                         trainInfo$seqParam[[j]],
                                         trainInfo$loop[j, trainInfo$constant,drop = FALSE])           
               results <- rbind(results, thisIter)
             },
             oob =
             {
               tmpModelFit <- do.call(createModel, argList)      
               tmpPerf <- switch(
                                 class(tmpModelFit)[1],
                                 randomForest = rfStats(tmpModelFit),
                                 RandomForest = cforestStats(tmpModelFit),
                                 bagEarth =, bagFDA = bagEarthStats(tmpModelFit),
                                 regbagg =, classbagg = ipredStats(tmpModelFit))
               performance[j, names(performance) %in% names(tmpPerf)] <- tmpPerf           
               
               
             })     
    }   
 
  paramNames <- substring(names(tuneGrid), 2)
  if(trControl$method != "oob")
    {     
      perResample <- poolByResample(results,
                                    tuneGrid,
                                    trControl$summaryFunction,
                                    perfNames,
                                    classLevels,
                                    method)
      performance <- summarize(perResample, tuneGrid)

      ## There are some cases where every resampled model
      ## failed for the tuning parameter(s). Catch these.
      goodResults <- complete.cases(performance)
      if(any(!goodResults))
        {
          if(trControl$verboseIter) cat(paste("These were", sum(!goodResults), "combinations with invalid results\n\n")) 
          performance <- performance[goodResults,]
        }

      pNames <- names(performance)
      pNames[pNames %in% names(tuneGrid)] <- paramNames
      names(performance) <- pNames
      
    } else {
      tmpLoop <- trainInfo$loop
      names(tmpLoop) <- substring(names(tmpLoop), 2)
      performance <- cbind(tmpLoop, performance)  
    }
  
  perfCols <- names(performance)
  perfCols <- perfCols[!(perfCols %in% paramNames)]

  ## Sort the tuning parameters form least complex to most complex
  performance <- byComplexity(performance, method)

  ## select the optimal set

  selectClass <- class(trControl$selectionFunction)[1]

  if(selectClass == "function")
    {
      bestIter <- trControl$selectionFunction(
                                              x = performance,
                                              metric = metric,
                                              maximize = maximize)
    }
  else {
    if(trControl$selectionFunction == "oneSE")
      {
        bestIter <- oneSE(
                          performance,
                          metric,
                          length(trControl$index),
                          maximize)
      } else {

        bestIter <- do.call(
                            trControl$selectionFunction,
                            list(
                                 x = performance,
                                 metric = metric,
                                 maximize = maximize))
      }
  }
  
  bestTune <- performance[bestIter, trainInfo$model$parameter, drop = FALSE]
  names(bestTune) <- paste(".", names(bestTune), sep = "") 
  
  if(trControl$method != "oob")
    {
      
      byResample <- switch(trControl$returnResamp,
                           none = NULL,
                           all =
                           {
                             out <- perResample
                             colnames(out) <- gsub("^\\.", "", colnames(out))
                             out
                           },
                           final =
                           {
                             out <- merge(bestTune, perResample)        
                             out <- out[,!(names(perResample) %in% names(tuneGrid))]
                             out
                           })                        
    } else {
      byResample <- NULL        
    } 

  ## $eorder rows of performance
  orderList <- list()
  for(i in seq(along = trainInfo$model$parameter))
    {
      orderList[[i]] <- performance[,trainInfo$model$parameter[i]]
    }
  names(orderList) <- trainInfo$model$parameter
  performance <- performance[do.call("order", orderList),]      
  
  
  ##------------------------------------------------------------------------------------------------------------------------------------------------------#

  finalModel <- createModel(
                            trainData, 
                            method = method, 
                            bestTune, 
                            obsLevels = classLevels, 
                            ...)
  
  ## Remove this and check for other places it is reference
  ## replaced by tuneValue
  if(method == "pls") finalModel$bestIter <- bestTune
  
  outData <- if(trControl$returnData) trainData else NULL
  
  ## In the case of pam, the data will need to be saved differently
  if(trControl$returnData & method == "pam")
    {
      finalModel$xData <- x
      finalModel$yData <- y
    }     
  
  structure(list(
                 method = method,
                 modelType = modelType,
                 results = performance,
                 bestTune = bestTune,
                 call = funcCall, 
                 dots = list(...),
                 metric = metric,
                 control = trControl,
                 finalModel = finalModel,
                 trainingData = outData,
                 resample = byResample,
                 perfNames = perfNames,
                 maximize = maximize
                 ), 
            class = "train")
}

train.formula <- function (form, data, ..., subset, na.action, contrasts = NULL) 
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) m$data <- as.data.frame(data)
  m$... <- m$contrasts <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  y <- model.response(m)
  res <- train(x, y, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("train", "train.formula")
  res
}
