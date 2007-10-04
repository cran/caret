"print.train" <-
function(x, digits = min(3, getOption("digits") - 3), printCall = TRUE, ...)
{
   stringFunc <- function (x) 
   {
       if (!is.character(x)) x <- as.character(x)
       numElements <- length(x)
       out <- if (length(x) > 0) {
           switch(min(numElements, 3), x, paste(x, collapse = " and "), 
               {
                   x <- paste(x, c(rep(",", numElements - 2), " and", 
                     ""), sep = "")
                   paste(x, collapse = " ")
               })
       } else ""
       out
   }   

   if(printCall)
   {
      cat("\nCall:\n")
      print(x$call)
   }
   cat("\n")
   
   if(!is.null(x$trainingData))
   {
      cat(
         dim(x$trainingData)[1], 
         " samples\n", 
         dim(x$trainingData)[2] - 1,
         " predictors\n\n",
         sep = "")    
   }
   
   if(!is.null(x$control$index))
   {
      resampleN <- unlist(lapply(x$control$index, length))
      numResamp <- length(resampleN)
      
      resampName <- switch(tolower(x$control$method),
         boot = paste("bootstrap (", numResamp, " reps)", sep = ""),
         cv = paste("cross-validation (", x$control$number, " fold)", sep = ""),
         lgocv = paste("leave group out cross-validation (", numResamp, " reps)", sep = ""))
     
      outLabel <- x$metric

      resampleN <- as.character(resampleN)
      if(numResamp > 5) resampleN <- c(resampleN[1:6], "...")
      cat("summary of", resampName, "sample sizes:\n   ", paste(resampleN, collapse = ", "), "\n\n")

   
      if(is.null(x$control$index) & x$control$number != numResamp)
      {
         cat(
            "(", 
            x$control$number - numResamp,
            " resamples were not returned",
            "by the lsf process)\n\n")
      }
   }
   tuneAcc <- x$results 
   tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]

   cat(x$control$method, "resampled training results")
   if(dim(tuneAcc)[1] > 1) cat(" across tuning parameters:\n") else cat("\n")
   cat("\n")

   
   if(dim(tuneAcc)[1] > 1)
   {
      optPerf <- if(x$metric != "RMSE") max( tuneAcc[,x$metric])
         else min(tuneAcc[,x$metric])   
      tuneAcc$Optimal <- ifelse(tuneAcc[,x$metric] == optPerf, "*", "")  
      if(sum(tuneAcc[,x$metric] == optPerf) > 1 & any(names(x$finalModel) == "tuneValue"))
      {
         optValues <- x$finalModel$tuneValue
         optValues <- paste(substring(names(optValues), 2), "=", optValues)
         optString <- paste(
            "\nThe final ",
            ifelse(ncol(x$finalModel$tuneValue) > 1, "values", "value"),
            " used in the model ",
            ifelse(ncol(x$finalModel$tuneValue) > 1, "were ", "was "),
            stringFunc(optValues),
            ".\n",
            sep = "")
      } else optString <- ""
   } else optString <- ""
      
	sdCols <- names(tuneAcc) %in% c("RMSESD", "RsquaredSD", "AccuracySD", "KappaSD")
	sdCheck <- unlist(lapply(
		tuneAcc[, sdCols, drop = FALSE],
		function(u) all(is.na(u))))
	if(any(sdCheck))
	{
		rmCols <- which(sdCols)[sdCheck]
		tuneAcc <- tuneAcc[, -rmCols]	
	}
      
      
   printList <- lapply(
      tuneAcc, 
      function(data, dig = digits)
      {
         if(is.numeric(data) & !is.factor(data)) out <- paste(signif(data, dig))
            else out <- as.character(data)
         out
      }) 
   printMat <- as.matrix(as.data.frame(printList))      
   rownames(printMat) <- rep("", dim(printMat)[1])
   colnames(printMat) <- gsub("SD", " SD", colnames(printMat))
          
   print(printMat, quote = FALSE, print.gap = 2)
   cat("\n")
   if(dim(tuneAcc)[1] > 1) cat(x$metric, "was used to select the optimal model\n")
   
   cat(optString)
   
   invisible(as.data.frame(printList))
}


