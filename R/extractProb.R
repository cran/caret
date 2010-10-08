

extractProb <- function(
                        models, 
                        testX = NULL, 
                        testY = NULL, 
                        unkX = NULL, 
                        unkOnly = !is.null(unkX) & is.null(testX), 
                        verbose = FALSE)
{

  objectNames <- names(models)
  if(is.null(objectNames)) objectNames <- paste("Object", 1:length(models), sep = "")

  
  if(any(unlist(lapply(models, function(x) !modelLookup(x$method)$probModel))))
    stop("only classification models that produce probabilities are allowed")
  
  obsLevels <- getClassLevels(models[[1]])

  trainX <- models[[1]]$trainingData[,!(names(models[[1]]$trainingData) %in% ".outcome")]
  trainY <- models[[1]]$trainingData$.outcome  

  if(verbose)
    {
      cat("Number of training samples:", length(trainY), "\n")
      cat("Number of test samples:    ", length(testY), "\n\n")
    }
  

  predProb <- predClass <- obs <- modelName <- dataType <- objName <- NULL
  if(!is.null(testX))
    {
      if(!is.data.frame(testX)) testX <- as.data.frame(testX)
      hasNa <- apply(testX, 1, function(data) any(is.na(data)))
      if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n"); flush.console()
    }
  
  for(i in seq(along = models))
    {
      modelFit <- models[[i]]$finalModel
      method <- models[[i]]$method
      if(verbose) cat("starting ", models[[i]]$method, "\n"); flush.console()         
      if(!unkOnly)
        {                                       
          if(is.null(models[[i]]$preProcess))
            {
              tempTrainPred <- predictionFunction(method, modelFit, trainX)
              tempTrainProb <- probFunction(method, modelFit, trainX)
            } else {
              ppTrain <- predict(models[[i]]$preProcess, trainX)
              tempTrainPred <- predictionFunction(method, modelFit, ppTrain)
              tempTrainProb <- probFunction(method, modelFit, ppTrain)
            }
          
          if(verbose) cat(models[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n"); flush.console()         
          
          predProb <- if(is.null(predProb)) tempTrainProb else rbind(predProb, tempTrainProb)      
          predClass <- c(predClass, as.character(tempTrainPred))         
          obs <- c(obs, as.character(trainY))
          modelName <- c(modelName, rep(models[[i]]$method, length(tempTrainPred)))
          objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
          dataType <- c(dataType, rep("Training", length(tempTrainPred)))         
          
                                        # Test Data         
          if(!is.null(testX) & !is.null(testY))
            {
              if(!is.data.frame(testX)) testX <- as.data.frame(testX)
              tempX <- testX
              tempY <- testY
              tempX$.outcome <- NULL
              if(is.null(models[[i]]$preProcess))
                {
                  tempTestPred <- predictionFunction(method, modelFit, tempX)
                  tempTestProb <- probFunction(method, modelFit, tempX)
                } else {
                  ppTest <- predict(models[[i]]$preProcess, tempX)
                  tempTestPred <- predictionFunction(method, modelFit, ppTest)
                  tempTestProb <- probFunction(method, modelFit, ppTest)
                }
              
              if(verbose) cat(models[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")         
              
              predProb <- if(is.null(predProb)) tempTestProb else rbind(predProb, tempTestProb)             
              predClass <- c(predClass, as.character(tempTestPred))         
              obs <- c(obs, as.character(testY))
              modelName <- c(modelName, rep(models[[i]]$method, length(tempTestPred)))
              objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))
              dataType <- c(dataType, rep("Test", length(tempTestPred)))                  
            }      
          
        }

                                        # Unknown Data   
      if(!is.null(unkX))
        {
          if(!is.data.frame(unkX)) unkX <- as.data.frame(unkX)
          tempX <- unkX
          tempX$.outcome <- NULL
          if(is.null(models[[i]]$preProcess))
            {
              tempUnkPred <- predictionFunction(method, modelFit, tempX)
              tempUnkProb <- probFunction(method, modelFit, tempX)
            } else {
              ppUnk <- predict(models[[i]]$preProcess, tempX)
              tempUnkPred <- predictionFunction(method, modelFit, ppUnk)
              tempUnkProb <- probFunction(method, modelFit, ppUnk)
            }  
          
          if(verbose) cat(models[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")         
          
          predProb <- if(is.null(predProb)) tempUnkProb else rbind(predProb, tempUnkProb)      
          predClass <- c(predClass, as.character(tempUnkPred))         
          obs <- c(obs, rep(NA, length(tempUnkPred)))
          modelName <- c(modelName, rep(models[[i]]$method, length(tempUnkPred)))
          objName <- c(objName, rep(objectNames[[i]], length(tempUnkPred)))
          dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))        
          
        }
      if(verbose) cat("\n")           
    }
  
  predClass <- factor(predClass, levels = obsLevels)
  obs <- factor(obs, levels = obsLevels)

  out <- data.frame(predProb)
  out$obs <- obs
  out$pred <- predClass
  out$model <- modelName
  out$dataType <- dataType
  out$object <- objName
  out
}

