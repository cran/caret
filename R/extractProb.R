

extractProb <- function(
   object, 
   testX = NULL, 
   testY = NULL, 
   unkX = NULL, 
   unkOnly = !is.null(unkX) & is.null(testX), 
   verbose = FALSE)
{

   if(any(unlist(lapply(object, function(x) x$modelType)) !=  "Classification"))
      stop("only classification models allowed")

   if(object[[1]]$method %in% c("svmradial", "svmpoly", "ctree", "cforest"))
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

   trainX <- object[[1]]$trainingData[,!(names(object[[1]]$trainingData) %in% ".outcome")]
   trainY <- object[[1]]$trainingData$.outcome  

   if(verbose)
   {
      cat("Number of training samples:", length(trainY), "\n")
      cat("Number of test samples:    ", length(testY), "\n\n")
   }
     

   predProb <- predClass <- obs <- modelName <- dataType <-  NULL
   if(!is.null(testX))
   {
      if(!is.data.frame(testX)) testX <- as.data.frame(testX)
      hasNa <- apply(testX, 1, function(data) any(is.na(data)))
      if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n"); flush.console()
   }
   
   for(i in seq(along = object))
   {
      modelFit <- object[[i]]$finalModel
      method <- object[[i]]$method
      if(verbose) cat("starting ", object[[i]]$method, "\n"); flush.console()         
      if(!unkOnly)
      {
         # Training Data
         tempTrainPred  <- predictionFunction(method, modelFit, trainX)
         tempTrainProb <- probFunction(method, modelFit, trainX)         
         if(verbose) cat(object[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n"); flush.console()         
         
         predProb <- if(is.null(predProb)) tempTrainProb else rbind(predProb, tempTrainProb)      
         predClass <- c(predClass, as.character(tempTrainPred))         
         obs <- c(obs, as.character(trainY))
         modelName <- c(modelName, rep(object[[i]]$method, length(tempTrainPred)))
         dataType <- c(dataType, rep("Training", length(tempTrainPred)))         
         
         # Test Data         
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

            tempTestPred  <- predictionFunction(method, modelFit, tempX)  
            tempTestProb <- probFunction(method, modelFit, tempX)       
            if(verbose) cat(object[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")         
            
            predProb <- if(is.null(predProb)) tempTestProb else rbind(predProb, tempTestProb)             
            predClass <- c(predClass, as.character(tempTestPred))         
            obs <- c(obs, as.character(testY))
            modelName <- c(modelName, rep(object[[i]]$method, length(tempTestPred)))
            dataType <- c(dataType, rep("Test", length(tempTestPred)))                  
         }      
         
      }

      # Unknown Data   
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
  
         tempUnkPred  <- predictionFunction(method, modelFit, tempX)
         tempUnkProb <- probFunction(method, modelFit, tempX)
      
         if(verbose) cat(object[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")         
         
         predProb <- if(is.null(predProb)) tempUnkProb else rbind(predProb, tempUnkProb)      
         predClass <- c(predClass, as.character(tempUnkPred))         
         obs <- c(obs, rep(NA, length(tempUnkPred)))
         modelName <- c(modelName, rep(object[[i]]$method, length(tempUnkPred)))
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
   out
}

