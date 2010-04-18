"train" <-
  function(x, ...){
    UseMethod("train")
  }

train.default <- function(x, y, 
                          method = "rf", 
                          ...,
                          weights = NULL,
                          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                          maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                          trControl = trainControl(),
                          tuneGrid = NULL, 
                          tuneLength = 3)
{

  funcCall <- match.call(expand.dots = TRUE)
  
  modelType <- if(is.factor(y)) "Classification"  else "Regression"

  ## There is a table (actually a data frame) that is produced by modelLookup with
  ## (multiple) rows for each model. There is one row per model parameter and columns
  ## for whether the model is used for regression, classification, the parameter name
  ## and label and whether the model can be fit sequentially (i.e. multiple models can
  ## be derived from the same R object).
  modelInfo <- modelLookup(method)
  
  if(modelType == "Classification")
    {     
      if(!any(modelInfo$forClass)) stop("wrong model type for classification")
      ## We should get and save the class labels to ensure that predictions are coerced      
      ## to factors that have the same levels as the original data. This is especially 
      ## important with multiclass systems where one or more classes have low sample sizes
      ## relative to the others
      classLevels <- levels(y)
      if(length(classLevels) > 2 & (method %in% c("gbm", "glmboost", "ada", "gamboost", "blackboost", "penalized", "glm",
                                                  "earth", "nodeHarvest", "glmrob", "plr", "GAMens", "rocc")))
        stop("This model is only implemented for two class problems")
      if(length(classLevels) < 3 & (method %in% c("vbmpRadial")))
        stop("This model is only implemented for 3+ class problems")      
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

  ## If they don't exist, make the data partitions for the resampling iterations.
  if(is.null(trControl$index)) trControl$index <- switch(
                                                         tolower(trControl$method),
                                                         oob = NULL,
                                                         cv = createFolds(y, trControl$number, returnTrain = TRUE),
                                                         loocv = createFolds(y, length(y), returnTrain = TRUE),
                                                         boot = createResample(y, trControl$number),
                                                         test = createDataPartition(y, 1, trControl$p),
                                                         lgocv = createDataPartition(y, trControl$number, trControl$p))
  
  ## Combine the features and classes into one df
  trainData <- as.data.frame(x)

  ## Check mode for some models
  if(!(method %in% c("rf", "rpart", "gbm", "treebag", "nb")))
    {
      isFactor <- lapply(trainData, is.factor)
      isCharacter <- lapply(trainData, is.character)
      if(any(unlist(isFactor))   | any(unlist(isCharacter)))  
        stop("All predictors must be numeric for this model. Use the formula interface: train(formula, data)") 
    }

  ## Add the outcome to the data passed into the functions
  trainData$.outcome <- y
  if(!is.null(weights)) trainData$.modelWeights <- weights

  ## If no default training grid is specified, get one. We have to pass in the formula
  ## and data for some models (rpart, pam, etc - see manual for more details)
  if(is.null(tuneGrid)) tuneGrid <- createGrid(method, tuneLength, trainData)

  ##------------------------------------------------------------------------------------------------------------------------------------------------------#

  ## For each tuning parameter combination, we will loop over them, fit models and generate predictions.
  ## We only save the predictions at this point, not the models (and in the case of method = "oob" we 
  ## only save the prediction summaries at this stage.
  
  ## trainInfo will hold the information about how we should loop to train the model and what types
  ## of parameters are used. 
  
  trainInfo <- tuneScheme(method, tuneGrid, trControl$method == "oob")
  paramCols <- paste(".", trainInfo$model$parameter, sep = "")

  if(trainInfo$scheme == "oob")
    {
      perfNames <- if(modelType == "Regression") c("RMSE", "Rsquared") else  c("Accuracy", "Kappa")    
    } else {
      ## get phoney performance to obtain the names of the outputs
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


  ## Now, we setup arguments to lapply (or similar functions) executed via do.call
  ## workerData will split up the data need for the jobs
  argList <- list(X =
                  workerData(
                             trainData,
                             trControl$index,
                             trainInfo, method,
                             classLevels,
                             trControl$workers,
                             trControl$verboseIter,
                             ...),
                  FUN = workerTasks)

  ## Append the extra objects needed to do the work (See the parallel examples in
  ## ?train to see examples
  if(!is.null(trControl$computeArgs)) argList <- c(argList, trControl$computeArgs)

  ## Get the predictions (or summaries for OOB)
  listOutput <- do.call(trControl$computeFunction, argList)

  if(trControl$method != "oob")
    {
      results <- do.call("rbind", listOutput)
    } else {
      performance <- cbind(trainInfo$loop, do.call("rbind", listOutput))
      colnames(performance) <- gsub("^\\.", "", colnames(performance))
    }

  paramNames <- names(tuneGrid)
  paramNames <- gsub("^\\.", "", paramNames)

  ## Now take the predictions and boil them down to performance matrics per tuning
  ## parameter and resampling combo.
  if(trControl$method != "oob")
    {     
      resampleResults <- getPerformance(results,
                                        paramCols,
                                        trControl$summaryFunction,
                                        classLevels,
                                        method,
                                        trControl$method == "LOOCV")
      perResample <- resampleResults$values
      performance <- resampleResults$results
    }
  
  perfCols <- names(performance)
  perfCols <- perfCols[!(perfCols %in% paramNames)]

  ## Sort the tuning parameters from least complex to most complex
  performance <- byComplexity(performance, method)

  ## select the optimal set
  selectClass <- class(trControl$selectionFunction)[1]

  ## Select the "optimal" tuning parameter.
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


  ## Based on the optimality criterion, select the tuning parameter(s)
  bestTune <- performance[bestIter, trainInfo$model$parameter, drop = FALSE]
  names(bestTune) <- paste(".", names(bestTune), sep = "") 

  ## Save some or all of the resampling summary metrics
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


  ## Reorder rows of performance
  orderList <- list()
  for(i in seq(along = trainInfo$model$parameter))
    {
      orderList[[i]] <- performance[,trainInfo$model$parameter[i]]
    }
  names(orderList) <- trainInfo$model$parameter
  performance <- performance[do.call("order", orderList),]      
  
  ## Make the final model based on the tuning results
  finalModel <- createModel(
                            trainData, 
                            method = method, 
                            bestTune, 
                            obsLevels = classLevels, 
                            ...)
  
  ## Remove this and check for other places it is reference
  ## replaced by tuneValue
  if(method == "pls") finalModel$bestIter <- bestTune

  ## To use predict.train and automatically use the optimal lambda,
  ## we need to save it
  if(method == "glmnet") finalModel$lambdaOpt <- bestTune$.lambda

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

train.formula <- function (form, data, ..., weights, subset, na.action, contrasts = NULL) 
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
  w <- as.vector(model.weights(m))
  res <- train(x, y, weights = w, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("train", "train.formula")
  res
}
