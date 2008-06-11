"print.train" <-
  function(x, digits = min(3, getOption("digits") - 3), printCall = TRUE, details = FALSE, ...)
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
      numParam <- nrow(caret:::modelLookup(x$method))

      finalTune <- x$bestTune
      names(finalTune) <- substring(names(finalTune), 2)

      optValues <- paste(names(finalTune), "=", finalTune)
      optString <- paste(
                         "\nThe final ",
                         ifelse(numParam, "values", "value"),
                         " used in the model ",
                         ifelse(numParam, "were ", "was "),
                         stringFunc(optValues),
                         ".\n",
                         sep = "")

      
      finalTune$Selected <- "*"

      tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
      tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""

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
  if(dim(tuneAcc)[1] > 1)
    {
      cat(x$metric, "was used to select the optimal model using")
      if(is.function(x$control$selectionFunction))
        {
          cat(" a custom selection rule.\n")
        } else {

          cat(
              switch(
                     x$control$selectionFunction,
                     best = paste(
                       " the",
                       ifelse(x$metric == "RMSE", "smallest", "largest"),
                       "value.\n"),
                     oneSE = " the one SE rule.\n",
                     tolerance = " a tolerance rule.\n"))
        }
    }
      
  cat(optString)
  
  if(details)
    {
      if(!(x$method %in% c("gbm", "treebag", "nb", "lvq", "knn")))
        {
          cat("\n----------------------------------------------------------\n")
          cat("\nThe final model:\n\n")
          switch(x$method,
                 lm =, nnet =, multinom =, pls =, earth =, 
                 bagEarth =, bagFDA = print(summary(x$finalModel)),
                 
                 rpart =, ctree =, cforest =,
                 glmboost =, gamboost =, blackboost =,
                 ada =, randomForest =,
                 svmradial =, svmpoly =, enet =, lasso =,
                 lda =, rda =, pamr =, gpls = print(x$finalModel),
                 fda = 
                 {
                   print(x$finalModel)
                   cat("\n Summary of Terms\n\n")
                   print(x$finalModel$fit)
                   
                 })
        }
    }
  
  
  invisible(as.data.frame(printList))
}


