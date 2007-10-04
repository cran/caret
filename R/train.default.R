
"train.default" <-
function(x, y, 
   method = "rf", 
   ..., 
   metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
   trControl = trainControl(),
   tuneGrid = NULL, 
   tuneLength = 3)
{

   funcCall <- match.call(expand.dots = TRUE)
   
   modelType <- if(is.factor(y)) "Classification"  else "Regression"
           
   if(modelType == "Classification")
   {     
      if(method %in% c("mars", "lasso", "lm", "earth")) stop("wrong model type for classification")
      # we should get and save the class labels to ensure that predictions are coerced      
      # to factors that have the same levels as the original data. This is especially 
      # important with multiclass systems where one or more classes have low sample sizes
      # relative to the others
      classLevels <- levels(y)
      if(length(classLevels) > 2 & (method %in% c("gbm", "glmboost", "ada", "gamboost", "blackboost")))
         stop("This model is only implimented for two class problems")
      if(!(metric %in% c("Accuracy", "Kappa"))) 
         stop(paste("Metric", metric, "not applicable for classification models"))
   } else {
      if(method %in% c("rda", "mda", "lvq", "gpls", "nb", "pam", "knn", "lda", "fda", "ada")) 
         stop("wrong model type for regression")
      if(!(metric %in% c("RMSE", "Rsquared"))) 
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
   
   # combine the features and classes into one df, as needed by ipred.
   trainData <- as.data.frame(x)

   #check mode for some models
   if(!(method %in% c("rf", "rpart", "gbm", "treebag", "nb")))
   {
      isFactor <- lapply(trainData, is.factor)
      isCharacter <- lapply(trainData, is.character)
      if(any(isFactor)   | any(isCharacter))  
         stop("All predictors must be numeric for this model") 
   }

   # add the outcome to the data passed into the functions
   trainData$.outcome <- y

   # if no default training grid is specified, get one. We have to pass in the formula
   # and data for some models (rpart, pam, etc - see manual fo rmore details)
   if(is.null(tuneGrid)) tuneGrid <- createGrid(method, tuneLength, trainData)
   
   # performance will be a container for the resampled performance
   if(modelType == "Regression")
   {
      performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(tuneGrid)[1]))   
      names(performance) <- c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")   

   } else {
      performance <- data.frame(matrix(NA, ncol = 4, nrow = dim(tuneGrid)[1]))   
      names(performance) <- c("Accuracy", "Kappa", "AccuracySD", "KappaSD")

   }
 
 	# not needed for oob
 	
   if(trControl$method != "oob") resamplePredictions <- vector(mode = "list", length = dim(tuneGrid)[1])
 
   # loop over the training grid combinations, train the models and return the
   # results 
   for(i in seq(along = tuneGrid[,1]))
   {
      if(trControl$verboseIter)
      {
         if(!all(is.null(tuneGrid[i,]))) cat("Iter", i, " Values:", paste(format(tuneGrid[i,,drop = FALSE]), collapse = ", "), "\n")
         flush.console() 
      }

      argList <- list(
         data = trainData,
         method = method,
         tuneValue = tuneGrid[i,, drop = FALSE],
         obsLevels = classLevels)
      argList <- append(argList, list(...))

		# for non-oob error rates, we will fit the model and return the held-out predictions
		if(trControl$method != "oob")
		{
	      resampleMatrix <- matrix(
	         NA,
	         ncol = length(trControl$index),
	         nrow=dim(trainData)[1])
	
	
	      for(m in seq(along = trControl$index))
	      {
	         trainDataInd <- trControl$index[[m]]
	         resampleMatrix[, m] <- resampleWrapper(argList, trainDataInd)   
	      }
              if(method == "pam") cat("\n")

              
	      if(modelType == "Classification")
	      {
	         resampleMatrix <- as.data.frame(resampleMatrix)
	         resampleMatrix <- as.data.frame(lapply(
	            resampleMatrix, 
	            function(data, y) factor(data, levels = levels(y)), 
	            y = y)) 
	      }
	                   
	      # we will post-process the results to get performance
	      # metrics per bootstrap sample or cv group, then save
	      # the averages and SDs of the metrics
	      resampleResults <- resampleSummary(
	         trainData$.outcome,
	         resampleMatrix,
	         trControl$index)
	         
	      performance[i,] <- resampleResults$metrics
	      resamplePredictions[[i]] <- resampleResults$data
      } else {
      	# here it the model and return the oob preformance measures
         tmpModelFit <- do.call(createModel, argList)      
         tmpPerf <- switch(
            class(tmpModelFit)[1],
            randomForest = rfStats(tmpModelFit),
            RandomForest = cforestStats(tmpModelFit),
            bagEarth =, bagFDA = bagEarthStats(tmpModelFit),
            regbagg =, classbagg = ipredStats(tmpModelFit))
         performance[i, names(performance) %in% names(tmpPerf)] <- tmpPerf      
            
      }
   }  

   # figure out the best combination (based on performance)
   # re-ft the model under this conditions (and anything specified
   # in the ... arguments)

   bestIter <- if(metric != "RMSE") which.max(performance[,metric])
      else which.min(performance[,metric])

   finalModel <- createModel(
      trainData, 
      method = method, 
      tuneGrid[bestIter,, drop = FALSE], 
      obsLevels = classLevels, 
      ...)
    
   # remove this and check for other places it is reference
   # replaced by tuneValue
   if(method == "pls") finalModel$bestIter <- tuneGrid[bestIter,, drop = FALSE]

   # save the resampling statistics where appropriate
   if(trControl$method != "oob")
   {
      #check for loo 
      if(any(nrow(trainData) - unlist(lapply(trControl$index, length)) == 1))
      {
         byResample <- NULL      
      } else {
         bestResamples <- resamplePredictions[[bestIter]]
         summaryStats <- by(bestResamples, bestResamples$group, function(x) postResample(x$pred, x$obs))
         byResample <- matrix(unlist(unclass(summaryStats)), ncol = length(summaryStats[[1]]), byrow = TRUE)
         byResample <- as.data.frame(byResample)
         colnames(byResample) <- names(summaryStats[[1]]) 
      }
   } else byResample <- NULL

   # remove the dot in the first position of the name
   names(tuneGrid) <-  substring(names(tuneGrid), 2)
   resultDataFrame <- cbind(tuneGrid, performance)
   
   
   outData <- if(trControl$returnData) trainData else NULL
   
   # in the case of pam, the data will need to be saved differently
   if(trControl$returnData & method == "pam")
   {
      finalModel$xData <- x
      finalModel$yData <- y
   }     
   
   structure(list(
      method = method,
      modelType = modelType,
      results = resultDataFrame, 
      call = funcCall, 
      dots = list(...),
      metric = metric,
      control = trControl,
      finalModel = finalModel,
      trainingData = outData,
      resample = byResample
      ), 
      class = "train")
}

