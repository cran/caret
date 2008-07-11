

extractPrediction <- function(
   object, 
   testX = NULL, 
   testY = NULL, 
   unkX = NULL, 
   unkOnly = !is.null(unkX) & is.null(testX), 
   verbose = FALSE)
{

   trainX <- object[[1]]$trainingData[,!(names(object[[1]]$trainingData) %in% ".outcome")]
   trainY <- object[[1]]$trainingData$.outcome  

   if(object[[1]]$method %in% c("svmradial", "svmpoly", "ctree", "ctree2", "cforest"))
   {
      obsLevels <- switch(object[[1]]$method,
         svmradial =, svmpoly =
         {
            library(kernlab)
            lev(object[[1]]$finalModel)
         },
   
         ctree =, cforest =
         {
            library(party)
            levels(object[[1]]$finalModel@data@get("response")[,1])
         })
   } else {
      obsLevels <- object[[1]]$finalModel$obsLevels
   }  


   if(verbose)
   {
      cat("Number of training samples:", length(trainY), "\n")
      cat("Number of test samples:    ", length(testY), "\n\n")
   }
     
   # do this differently!
   pred <- obs <- modelName <- dataType <-  NULL
   if(!is.null(testX))
   {
      if(!is.data.frame(testX)) testX <- as.data.frame(testX)
      hasNa <- apply(testX, 1, function(data) any(is.na(data)))
      if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n")
   }
   
   for(i in seq(along = object))
   {
      modelFit <- object[[i]]$finalModel
      method <- object[[i]]$method
      
      if(!unkOnly)
      {
     
         #now generate predictions on the training data from the final model
         tempTrainPred <- predictionFunction(method, modelFit, trainX)
      
         if(verbose) cat(object[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n")         
         
         if(object[[i]]$modelType == "Classification")
         {
            pred <- c(pred, as.character(tempTrainPred))
            obs <- c(obs, as.character(trainY))
         } else {
            pred <- c(pred, tempTrainPred)
            obs <- c(obs, trainY)      
         }
         
         modelName <- c(modelName, rep(object[[i]]$method, length(tempTrainPred)))
         dataType <- c(dataType, rep("Training", length(tempTrainPred)))
         
         if(!is.null(testX) & !is.null(testY))
         {
            if(object[[i]]$method %in% c("rpart", "treebag"))
            {
               tempX <- testX
               tempY <- testY
            } else {
               tempX <- testX[!hasNa,]
               tempY <- testY[!hasNa]         
            }            
     
            tempTestPred <- predictionFunction(method, modelFit, tempX)
            
            if(verbose) cat(object[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")         
         
            if(object[[i]]$modelType == "Classification")
            {
               pred <- c(pred, as.character(tempTestPred))
               obs <- c(obs, as.character(tempY))    
            } else {
               pred <- c(pred, tempTestPred)   
               obs <- c(obs, tempY) 
            }
           
            modelName <- c(modelName, rep(object[[i]]$method, length(tempTestPred)))
            dataType <- c(dataType, rep("Test", length(tempTestPred)))   
            
         }
         if(verbose) cat("\n")
      }
      if(!is.null(unkX))
      {
         if(object[[i]]$method %in% c("rpart", "treebag"))
         {
            tempX <- unkX
         } else {
            if(!is.data.frame(unkX)) unkX <- as.data.frame(unkX)
            hasNa <- apply(unkX, 1, function(data) any(is.na(data)))         
            tempX <- unkX[!hasNa,]
         }            
  
         tempUnkPred <- predictionFunction(method, modelFit, tempX)
         
         if(verbose) cat(object[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")         
      
         if(object[[i]]$modelType == "Classification")
         {
            pred <- c(pred, as.character(tempUnkPred))
            obs <- c(obs, rep("", length(tempUnkPred)))    
         } else {
            pred <- c(pred, tempUnkPred)   
            obs <- c(obs, rep(NA, length(tempUnkPred)))    
         }
        
         modelName <- c(modelName, rep(object[[i]]$method, length(tempUnkPred)))
         dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))   
         
      }
      if(verbose) cat("\n")      
   }
   if(object[[1]]$modelType == "Classification")
   {   
      pred <- factor(pred, levels = obsLevels)
      obs <- factor(obs, levels = obsLevels)
   }
   
   data.frame(obs = obs, pred = pred, model = modelName, dataType = dataType)
}

