"train" <-
  function(x, ...){
    UseMethod("train")
  }

train.default <- function(x, y, 
                          method = "rf",
                          preProcess = NULL,
                          ...,
                          weights = NULL,
                          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                          maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                          trControl = trainControl(),
                          tuneGrid = NULL, 
                          tuneLength = 3)
{

  startTime <- proc.time()
  funcCall <- match.call(expand.dots = TRUE)
  
  modelType <- if(is.factor(y)) "Classification"  else "Regression"

  ## There is a table (actually a data frame) that is produced by modelLookup with
  ## (multiple) rows for each model. There is one row per model parameter and columns
  ## for whether the model is used for regression, classification, the parameter name
  ## and label and whether the model can be fit sequentially (i.e. multiple models can
  ## be derived from the same R object).
  modelInfo <- modelLookup(method)

  if(method %in% c("logreg", "logforest", "logicBag"))
    {
      binaryCheck <- unlist(lapply(x, function(x) any(!(x %in% c(0, 1)))))
      if(any(binaryCheck)) stop(
                                paste("When using logic regression, train() is",
                                      "only setup to use predictors that are",
                                      "either 0 or 1"))
    }

  if(!is.null(preProcess) && !(all(preProcess %in% c("center", "scale", "pca", "ica", "BoxCox", "spatialSign", "knnImpute", "bagImpute", "range")))) 
    stop('pre-processing methods are limited to center, scale, range, pca, ica, knnImpute, bagImpute and spatialSign')

  
  if(modelType == "Classification")
    {     
      if(!any(modelInfo$forClass)) stop("wrong model type for classification")
      ## We should get and save the class labels to ensure that predictions are coerced      
      ## to factors that have the same levels as the original data. This is especially 
      ## important with multiclass systems where one or more classes have low sample sizes
      ## relative to the others
      classLevels <- levels(y)

      if(any(classLevels != make.names(classLevels)))
         {
           warning(paste("At least one of the class levels are not valid R variables names;",
                         "This may cause errors if class probabilities are generated because",
                         "the variables names will be converted to:",
                         paste(make.names(classLevels), collapse = ", ")))
         }
      
      if(length(classLevels) > 2 & (method %in% c("gbm", "glmboost", "ada", "gamboost", "blackboost", "penalized", "glm",
                                                  "earth", "nodeHarvest", "glmrob", "plr", "GAMens", "rocc",
                                                  "logforest", "logreg", "gam", "gamLoess", "gamSpline")))
        stop("This model is only implemented for two class problems")
      if(length(classLevels) < 3 & (method %in% c("vbmpRadial")))
        stop("This model is only implemented for 3+ class problems")      
      if(metric %in% c("RMSE", "Rsquared")) 
        stop(paste("Metric", metric, "not applicable for classification models"))
      if(trControl$classProbs & any(!modelInfo$probModel))
        {
          warning("Class probabilities were requested for a model that does not implement them")
          trControl$classProbs <- FALSE
        }
      if(method %in% c("svmLinear", "svmRadial", "svmPoly") & any(names(list(...)) == "class.weights"))
         warning("since class weights are requested, SVM class probabilities cannot be generated")
         
         
    } else {
      if(!any(modelInfo$forReg)) stop("wrong model type for regression")
      if(metric %in% c("Accuracy", "Kappa")) 
        stop(paste("Metric", metric, "not applicable for regression models"))         
      classLevels <- NA
      if(trControl$classProbs)
        {
          warning("cannnot compute class probabilities for regression")
          trControl$classProbs <- FALSE
        }   
    }
  
  if(trControl$method == "oob" & !(method %in% c("rf", "treebag", "cforest", "bagEarth", "bagFDA")))
    stop("for oob error rates, model bust be one of: rf, cforest, bagEarth, bagFDA or treebag")

  ## If they don't exist, make the data partitions for the resampling iterations.
  if(is.null(trControl$index)) trControl$index <- switch(
                                                         tolower(trControl$method),
                                                         oob = NULL,
                                                         cv = createFolds(y, trControl$number, returnTrain = TRUE),
                                                         repeatedcv = createMultiFolds(y, trControl$number, trControl$repeats),
                                                         loocv = createFolds(y, length(y), returnTrain = TRUE),
                                                         boot =, boot632 = createResample(y, trControl$number),
                                                         test = createDataPartition(y, 1, trControl$p),
                                                         lgocv = createDataPartition(y, trControl$number, trControl$p))


  if(trControl$method != "oob" & is.null(trControl$index)) names(trControl$index) <- prettySeq(trControl$index)
  
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
  if(is.null(tuneGrid))
    {
      tuneGrid <- createGrid(method, tuneLength, trainData)
    } else {
      if(is.function(tuneGrid))
        {
          if(length(formals(tuneGrid)) == 2
             && names(formals(tuneGrid)) == c("len", "data"))
            {
              tuneGrid <- tuneGrid(tuneLength, trainData)
            } else stop("If a function, tuneGrid should have arguments len and data")
        } else {
          ## Check tuneing parameter names
          tuneNames <- modelLookup(method)$parameter
          tuneNames <- paste(".", sort(tuneNames), sep = "")
          goodNames <- all.equal(tuneNames, sort(names(tuneGrid)))
          if(!is.logical(goodNames) || !goodNames)
            stop(paste("The tuning parameter grid must have columns",
                       paste(tuneNames, collapse = ", ")))
        }
    }


  ##------------------------------------------------------------------------------------------------------------------------------------------------------#

  ## For each tuning parameter combination, we will loop over them, fit models and generate predictions.
  ## We only save the predictions at this point, not the models (and in the case of method = "oob" we 
  ## only save the prediction summaries at this stage.
  
  ## trainInfo will hold the information about how we should loop to train the model and what types
  ## of parameters are used.

  ## There are two types of methods to build the models: "basic" means that each tuning parameter
  ## combination requires it's own model fit and "seq" where a single model fit can be used to
  ## get predictions for multiple tuning parameters.

  ## The tuneScheme() function is in miscr.R and it helps define the following:
  ##   - A data frame called "loop" with columns for parameters and a row for each model to be fit.
  ##     For "basic" models, this is the same as the tuning grid. For "seq" models, it is only
  ##     the subset of parameters that need to be fit
  ##   - A list called "seqParam". If "basic", it is NULL. For "seq" models, it is a list. Each list
  ##     item is a data frame of the parameters that need to be varied for the corresponding row of
  ##     the loop oject.
  ##
  ## For example, for a gbm model, our tuning grid might be:
  ##    .interaction.depth .n.trees .shrinkage
  ##                     1       50        0.1
  ##                     1      100        0.1
  ##                     2       50        0.1
  ##                     2      100        0.1
  ##                     2      150        0.1
  ##
  ## For this example:
  ## 
  ##   loop:
  ##   .interaction.depth .shrinkage .n.trees
  ##                    1        0.1      100
  ##                    2        0.1      150
  ##
  ##   seqParam:
  ##   [[1]]
  ##     .n.trees
  ##           50
  ## 
  ##   [[2]]
  ##     .n.trees
  ##           50
  ##          100
  ## 
  ## A simplified version of predictionFunction() would have the following gbm section:
  ##
  ##     # First get the predicitons with the value of n.trees as given in the current
  ##     # row of loop
  ##     out <- predict(modelFit,
  ##                    newdata,
  ##                    type = "response",
  ##                    n.trees = modelFit$tuneValue$.n.trees)
  ##
  ##     # param is the current value of seqParam. In normal predction mode (i.e
  ##     # when using predict.train), param = NULL. When called within train()
  ##     # with this model, it will have the other values for n.trees.
  ##     # In this case, the output of the function is a list of predictions
  ##     # These values are deconvoluted in workerTasks() in misc.R
  ##     if(!is.null(param))
  ##       {
  ##         tmp <- vector(mode = "list", length = nrow(param) + 1)
  ##         tmp[[1]] <- out
  ##         
  ##         for(j in seq(along = param$.n.trees))
  ##           {   
  ##             tmp[[j]]  <- predict(modelFit,
  ##                                  newdata,
  ##                                  type = "response",
  ##                                  n.trees = param$.n.trees[j])
  ##           }
  ##         out <- tmp
  ##
  
  trainInfo <- tuneScheme(method, tuneGrid, trControl$method == "oob")
  paramCols <- paste(".", trainInfo$model$parameter, sep = "")

  if(trainInfo$scheme == "oob")
    {
      perfNames <- if(modelType == "Regression") c("RMSE", "Rsquared") else  c("Accuracy", "Kappa")    
    } else {
      ## get phoney performance to obtain the names of the outputs
      testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                               obs = sample(y, min(10, length(y))))

      if(trControl$classProbs)
        {
          for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
        }
      
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

  ## TODO: pass a flag into argList that is LOO = TRUE/FALSE. If TRUE, then
  ## run default summary on the output of listOutput. Otherwise, the returned
  ## value will be the summarized performance metrics

  ## Now, we setup arguments to lapply (or similar functions) executed via do.call
  ## workerData will split up the data need for the jobs
  argList <- list(X =
                  workerData(
                             data = trainData,
                             ctrl = trControl,
                             loop = trainInfo,
                             method = method,
                             lvls = classLevels,
                             pp = preProcess,
                             workers = trControl$workers,
                             caretVerbose = trControl$verboseIter,
                             ...),
                  FUN = workerTasks)

  ## Append the extra objects needed to do the work (See the parallel examples in
  ## ?train to see examples
  if(!is.null(trControl$computeArgs)) argList <- c(argList, trControl$computeArgs)

  ## Get the predictions (or summaries for OOB)
  listOutput <- do.call(trControl$computeFunction, argList)

  if(trControl$method != "oob")
    {
      resampleResults <- rbind.fill(listOutput)
      colnames(resampleResults) <- gsub("^\\.", "", colnames(resampleResults))
      if(trControl$method == "LOOCV")
        {
          paramValues <- resampleResults[, trainInfo$model$param, drop = FALSE]
          allVars <- factor(apply(paramValues, 1, function(x) paste(x, collapse = ":")))
          resampleResults <- split(resampleResults, allVars)
          resampleResults <- lapply(resampleResults,
                                    looSummary,
                                    func = trControl$summaryFunction,
                                    param = trainInfo$model$param)
          resampleResults <- rbind.fill(resampleResults)
          

        }
      performance <- performanceSummary(resampleResults,
                                        trainInfo$model$param,
                                        trControl$method)
    } else {
      performance <- rbind.fill(listOutput)
      colnames(performance) <- gsub("^\\.", "", colnames(performance))
    }

  if(trControl$method == "boot632")
    {
      if(trControl$verboseIter)
        {
          cat("Calculating apparent performance values\n")
          flush.console()
        }
      argList$X <- lapply(argList$X,
                          function(object)
                          {
                            object$index <- list(Apparent = seq(along = object$data$.outcome))
                            object$caretVerbose <- FALSE
                            object
                          })
      apparent <-   do.call(trControl$computeFunction, argList)
      apparent <- do.call("rbind", apparent)
      colnames(apparent) <- gsub("^\\.", "", colnames(apparent))
      for(p in seq(along = perfNames))
        {
          const <- 1-exp(-1)
          performance[, perfNames[p]] <- (const * performance[, perfNames[p]]) +  ((1-const) * apparent[, perfNames[p]])
        }
    }

  
  paramNames <- trainInfo$model$param

  if(trControl$verboseIter)
    {
      cat("Aggregating results\n")
      flush.console()
    }
 
  perfCols <- names(performance)
  perfCols <- perfCols[!(perfCols %in% paramNames)]

  ## Sort the tuning parameters from least complex to most complex
  performance <- byComplexity(performance, method)

  if(trControl$verboseIter)
    {
      mod <- modelLookup(method)
      if(!all(mod$label == "none"))
        {
          cat("Selecting tuning parameters\n")
          flush.console()
        }
    }
  
  ## select the optimal set
  selectClass <- class(trControl$selectionFunction)[1]

  ## Select the "optimal" tuning parameter.
  if(selectClass == "function")
    {
      bestIter <- trControl$selectionFunction(x = performance,
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

  ## Save some or all of the resampling summary metrics
  if(!(trControl$method %in% c("LOOCV", "oob")))
    {
      
      byResample <- switch(trControl$returnResamp,
                           none = NULL,
                           all =
                           {
                             out <- resampleResults
                             colnames(out) <- gsub("^\\.", "", colnames(out))
                             out
                           },
                           final =
                           {
                             out <- merge(bestTune, resampleResults)
                             out <- out[,!(names(out) %in% names(tuneGrid))]
                             out
                           })                        
    } else {
      byResample <- NULL        
    } 

  names(bestTune) <- paste(".", names(bestTune), sep = "")   

  ## Reorder rows of performance
  orderList <- list()
  for(i in seq(along = trainInfo$model$parameter))
    {
      orderList[[i]] <- performance[,trainInfo$model$parameter[i]]
    }
  names(orderList) <- trainInfo$model$parameter
  performance <- performance[do.call("order", orderList),]      

  if(trControl$verboseIter)
    {
      cat("Fitting model on full training set\n")
      flush.console()
    }
    
  ## Make the final model based on the tuning results
  finalTime <- system.time(
                           finalModel <- createModel(data = trainData, 
                                                     method = method, 
                                                     tuneValue = bestTune, 
                                                     obsLevels = classLevels,
                                                     pp = list(options = preProcess,
                                                       thresh = trControl$PCAthresh,
                                                       ica = trControl$ICAcomp,
                                                       k = trControl$k),
                                                     ...))

  ## get pp info
  pp <- finalModel$preProc
  finalModel <- finalModel$fit
  
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

  endTime <- proc.time()
  times <- list(everything = endTime - startTime,
                final = finalTime)
  
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
                 preProcess = pp,
                 trainingData = outData,
                 resample = byResample,
                 perfNames = perfNames,
                 maximize = maximize,
                 times = times
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
