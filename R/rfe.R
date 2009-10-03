## todo:
## - re-write most of the documentation
## - test with many models
## - convert outer resample to lapply for easy parallelization
## - check lda ranking function
## - write man pages for lattice functions

rfeIter <- function(x, y,
                    testX, testY, sizes,
                    rfeControl = rfeControl(), ...)
{
  if(is.null(colnames(x))) stop("x must have column names")

  if(is.null(testX) | is.null(testY)) stop("a test set must be specified")
  if(is.null(sizes)) stop("please specify the number of features")

  predictionMatrix <- matrix(NA, nrow = length(testY), ncol = length(sizes))
  .x <- x
  .tx <- testX
  p <- ncol(x)

  sizeValues <- sort(unique(c(sizes, ncol(x))),
                     decreasing = TRUE)
  
  finalVariables <- vector(length(sizeValues), mode = "list")
  
  for(k in seq(along = sizeValues))
    {
      
      if(rfeControl$verbose) cat("  Fitting subset size:\t", sizeValues[k], "\n")
      flush.console()
      
      fitObject <- rfeControl$functions$fit(.x, y,
                                            first = p == ncol(.x),
                                            last = FALSE,
                                            ...)  

      modelPred <- data.frame(pred = rfeControl$functions$pred(fitObject, .tx),
                              obs = testY,
                              subset = sizeValues[k])
      
      rfePred <- if(k == 1) modelPred else rbind(rfePred, modelPred)


      if(!exists("modImp")) ##todo: get away from this since it finds object in other spaces
        {
          if(rfeControl$verbose) cat("  Computing importance\n")
          modImp <- rfeControl$functions$rank(fitObject, .x, y)
        } else {
          if(rfeControl$rerank)
            {
              if(rfeControl$verbose) cat("  Recomputing importance\n")              
              modImp <- rfeControl$functions$rank(fitObject, .x, y)
            }
        }

      retained <- as.character(modImp$var)[1:sizeValues[k]]
      .x  <-   x[, retained, drop = FALSE]
      .tx <- .tx[, retained, drop = FALSE]
      finalVariables[[k]] <- subset(modImp, var %in% retained)
      
    }
  names(rfePred) <- c("pred", "obs", "subset")
  list(finalVariables = finalVariables, pred = rfePred)

}

######################################################################
######################################################################

## This function will be executed to do the RFE iterations over
## different resampling iterations.

## The input is a single list that has elelemnts for:
##
## - indices:  a list of indices for which samples are in the training
##             set. This can include more than one resampling iteration
## - x: the full data set of predictors
## - y: the full set of outcomes
## - cntl: output from rfeControl
## - sizes: a vector of number of predictors

rfeWrapper <- function(X)
  {
    ## iterate over the resampling iterations
    index <- X$index
    X$index <- NULL
    out <- vector(mode = "list", length = length(index))
    for(i in seq(along = index))
      {
        out[[i]] <- do.call("rfeChunk",
                            c(list(inTrain= index[[i]]), X))
      }
    out
  }

######################################################################
######################################################################

rfeChunk <- function(inTrain, x, y, cntl, sizes, ...)
  {
    j <- which(unlist(lapply(cntl$index, function(x, y) all(x == y), y = inTrain)))
    if(length(j) == 0) stop("can't figure out which resample iteration this is")

    if(cntl$verbose) cat("\nExternal resampling iter:\t", j, "\n")
    flush.console()
    
    inTrainX <- x[inTrain, ]
    outTrainX <- x[-inTrain, ]
    inTrainY <- y[inTrain]
    outTrainY <- y[-inTrain]      
    rfeResults <- rfeIter(inTrainX, inTrainY,
                          outTrainX, outTrainY,
                          sizes,
                          cntl,
                          ...)
    
    rfeResults$pred$resampleIter <- rep(j, nrow(rfeResults$pred))

    out <- list(pred = rfeResults$pred,
                selectedVars = rfeResults$finalVariables)
    out
  }

######################################################################
######################################################################

rfe <- function (x, ...) UseMethod("rfe")

"rfe.default" <-
  function(x, y,
           sizes = 2^(2:4),
           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
           maximize = ifelse(metric == "RMSE", FALSE, TRUE),
           rfeControl = rfeControl(), ...)
{
  funcCall <- match.call(expand.dots = TRUE)
  require(caret)

  numFeat <- ncol(x)

  if(is.null(rfeControl$index))
    rfeControl$index <- switch(tolower(rfeControl$method),
                               cv = createFolds(y, rfeControl$number, returnTrain = TRUE),
                               loocv = createFolds(y, length(y), returnTrain = TRUE),
                               boot = createResample(y, rfeControl$number),
                               test = createDataPartition(y, 1, rfeControl$p),
                               lgocv = createDataPartition(y, rfeControl$number, rfeControl$p))
  

  sizeValues <- sort(unique(sizes))
  sizeValues <- sizeValues[sizeValues <= ncol(x)]


  ## check summary function and metric
  test <- rfeControl$functions$summary(data.frame(obs = y, pred = sample(y)))
  if(!(metric %in% names(test))) stop("the specified metric is not produced by the summary function")
  perfNames <- names(test)

  
  selectedVars <- vector(mode = "list", length = length(rfeControl$index))

### todo: do this over lapply for possible parallelization

  ## need to setup lists for each worker (!= each task)

  tmp <- repList(
                 list(x = x,
                      y = y,
                      cntl = rfeControl,
                      sizes = sizeValues,
                      ...),
                 rfeControl$workers) ## define this var

  indexSplit <- splitIndicies(length(rfeControl$index),
                              rfeControl$workers)
  for(i in seq(along = tmp)) tmp[[i]]$index <- rfeControl$index[indexSplit == i]
  
  ## Now, we setup arguments to lapply (or similar functions) executed via do.call
  ## workerData will split up the data need for the jobs
  argList <- list(X = tmp,
                  FUN = rfeWrapper)

  ## Append the extra objects needed to do the work (See the parallel examples in
  ## ?train to see examples
  if(!is.null(rfeControl$computeArgs)) argList <- c(argList, rfeControl$computeArgs)
 
  rfeResults <- do.call(rfeControl$computeFunction, argList)
  
  rfePred <- lapply(rfeResults,
                    function(x) lapply(x,
                                       function(y) y$pred))[[1]]
  rfePred <- do.call("rbind", rfePred)

  selectedVars <- lapply(rfeResults,
                    function(x) lapply(x,
                                       function(y) y$selectedVars))[[1]]

  #########################################################################

  subsets <- sort(unique(rfePred$subset), decreasing = TRUE)

  externPerf <- matrix(NA, ncol = 2* length(perfNames) + 1, nrow = length(subsets))
  colnames(externPerf) <- c("Variables",
                            perfNames,
                            paste(perfNames, "SD", sep = ""))
  
  
  for(i in seq(along = subsets))
    {
      dataSubset <- subset(
                           rfePred,
                           subset == subsets[i],
                           drop = FALSE)
      dataSubset$resampleIter <- factor(dataSubset$resampleIter)
      byResample <- split(dataSubset, dataSubset$resampleIter)
      resampleResults <- lapply(
                                byResample,
                                rfeControl$functions$summary)
      resampleResults <- t(as.data.frame(resampleResults))
      
      externPerf[i, -1] <-  c(apply(resampleResults, 2, mean, na.rm = TRUE),
                              apply(resampleResults, 2, sd  , na.rm = TRUE))
      externPerf[i, 1] <- subsets[i]
      if(rfeControl$returnResamp != "none")
        {
          resampleResults <- cbind(resampleResults,
                                   rep(subsets[i], nrow(resampleResults)))
          resamples <- if(i == 1) resampleResults else rbind(resamples, resampleResults)
        }
    }

  externPerf <- externPerf[order(externPerf[,"Variables"]),]
  
  bestSubset <- rfeControl$functions$selectSize(x = externPerf,
                                                metric = metric,
                                                maximize = maximize)

  bestVar <- rfeControl$functions$selectVar(selectedVars, bestSubset)  

  fit <- rfeControl$functions$fit(x[, bestVar, drop = FALSE],
                                  y,
                                  first = FALSE,
                                  last = TRUE,
                                  ...)

  resamples <- switch(rfeControl$returnResamp,
                      none = NULL, 
                      all = {
                        out <- resamples
                        colnames(out)[ncol(out)] <- "Variables"
                        rownames(out) <- NULL
                        out
                      },
                      final = {
                        out <- resamples[resamples[,ncol(resamples)]  == bestSubset,,drop = FALSE]
                        colnames(out)[ncol(out)] <- "Variables"
                        rownames(out) <- NULL
                        out
                      })


#########################################################################
  ## Now, based on probability or static ranking, figure out the best vars
  ## and the best subset size and fit final model
  
  structure(
            list(
                 pred = if(rfeControl$saveDetails) rfePred else NULL,
                 variables = if(rfeControl$saveDetails) selectedVars else NULL,
                 results = as.data.frame(externPerf),
                 bestSubset = bestSubset,
                 fit = fit,
                 optVariables = bestVar,
                 optsize = bestSubset,
                 call = funcCall,
                 control = rfeControl,
                 resample = resamples,
                 metric = metric,
                 maximize = maximize,
                 dots = list(...)),
            class = "rfe")
}

rfe.formula <- function (form, data, ..., subset, na.action, contrasts = NULL) 
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
  res <- rfe(as.data.frame(x), y, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("rfe", "rfe.formula")
  res
}

######################################################################
######################################################################

print.rfe <- function(x, top = 5, digits = max(3, getOption("digits") - 3), ...)
{

  cat("\nRecursive feature selection\n\n")

  cat("Outer resamping method was",
      x$control$number,
      "iterations of",
      switch(x$control$method,
             "boot" = "the bootstrap.",
             "LGOCV" = "leave group out cross-validation.",
             "LOOCV" = "leave one out cross-validation.",
             "cv" = "cross-validation."),
      "\n")

  cat("\nResampling perfromance over subset size:\n\n")
  x$results$Selected <- ""
  x$results$Selected[x$results$Variables == x$bestSubset] <- "*"
  print(format(x$results, digits = digits), row.names = FALSE)
  cat("\n")

  cat("The top ",
      min(top, x$bestSubset),
      " variables (out of ",
      x$bestSubset,
      "):\n   ",
      paste(x$optVariables[1:min(top, x$bestSubset)], collapse = ", "),
      "\n\n",
      sep = "")

  invisible(x)
}

######################################################################
######################################################################

plot.rfe <- function (x,
                      plotType = "size",
                      metric = x$metric,
                      digits = getOption("digits") - 5,
                      xTrans = NULL,
                      ...)
{
  switch(plotType,
         size =
         {
           x$results$Selected <- ""
           x$results$Selected[x$results$Variables == x$bestSubset] <- "*"
           
           results <- x$results[, colnames(x$results) %in% c("Variables", "Selected", metric)]
           metric <- metric[which(metric %in% colnames(results))]
           
           plotForm <- as.formula(paste(metric, "~ Variables"))
           panel.profile <- function(x, y, groups, ...)
             {
               panel.xyplot(x, y, ...)
               panel.xyplot(x[groups == "*"], y[groups == "*"], pch = 16)
             }
           
           out <- xyplot(plotForm, data = results, groups = Selected, panel =  panel.profile, ...)
         })
  out
}

######################################################################
######################################################################

rfeControl <- function(functions = NULL,
                       rerank = FALSE,
                       method = "boot",
                       saveDetails = FALSE,
                       number = ifelse(method == "cv", 10, 25),
                       verbose = TRUE,
                       returnResamp = "all",
                       p = .75,
                       index = NULL,
                       workers = 1,
                       computeFunction = lapply,
                       computeArgs = NULL)
{
  list(
       functions = if(is.null(functions)) caretFuncs else functions,
       rerank = rerank,
       method = method,
       saveDetails = saveDetails,
       number = number,
       returnResamp = returnResamp,
       verbose = verbose,
       p = p,
       index = index,
       workers = workers,
       computeFunction = computeFunction,
       computeArgs = computeArgs)
}

######################################################################
######################################################################
## some built-in functions for certain models

pickSizeBest <- function(x, metric, maximize)
  {
    best <- if(maximize) which.max(x[,metric]) else which.min(x[,metric])
    min(x[best, "Variables"])
  }

pickSizeTolerance <- function(x, metric, tol = 1.5, maximize)
  {
    if(!maximize)
      {
        best <- min(x[,metric])  
        perf <- (x[,metric] - best)/best * 100
      } else {
        best <- max(x[,metric])  
        perf <- (x[,metric] - best)/best * -100
      }
    flag <- perf <= tol
    min(x[flag, "Variables"])
  }



pickVars <- function(y, size)
  {
    imp <- lapply(y, function(x) x[[1]])
    imp <- do.call("rbind", imp)
    finalImp <- aggregate(imp$Overall, list(var = imp$var), mean, na.rm = TRUE)
    finalImp <- finalImp[order(finalImp$x, decreasing = TRUE),]
    as.character(finalImp$var[1:size])
  }


caretFuncs <- list(summary = defaultSummary,
                   fit = function(x, y, first, last, ...) train(x, y, ...),
                   pred = function(object, x)
                   {
                     modelPred <- extractPrediction(
                                                    list(object),
                                                    unkX = x,
                                                    unkOnly = TRUE)
                     modelPred <- modelPred[modelPred$dataType == "Unknown",]
                     modelPred$pred
                   },
                   rank = function(object, x, y)
                   {
                     vimp <- varImp(object, scale = FALSE)$importance
                     vimp <- vimp[
                                  order(vimp[,1], decreasing = TRUE)
                                  ,,drop = FALSE]
                     if(all(levels(y) %in% colnames(vimp)))
                       {
                         avImp <- apply(vimp[, levels(y), drop = TRUE],
                                        1,
                                        mean)
                         vimp$Overall <- avImp
                       }
                     vimp$var <- rownames(vimp)
                     vimp
                   },
                   selectSize = pickSizeBest,
                   selectVar = pickVars
                   )



## write a better imp sort function
ldaFuncs <- list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   library(MASS)
                   lda(x, y, ...)
                 },
                 pred = function(object, x)
                 {
                   predict(object, x)$class
                 },
                 rank = function(object, x, y)
                 {
                   vimp <- filterVarImp(x, y, TRUE)
                   avImp <- apply(vimp, 1, mean)
                   vimp <- vimp[
                                order(avImp, decreasing = TRUE)
                                ,]
                   
                   vimp <- as.data.frame(vimp)[,1,drop = FALSE]
                   vimp$var <- rownames(vimp)
                   names(vimp) <- "Overall"
                   
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars
                 )


treebagFuncs <- list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...)
                     {
                       library(ipred)
                       ipredbagg(y, x, ...)
                     },
                     pred = function(object, x)
                     {
                       predict(object, x)
                     },
                     rank = function(object, x, y)
                     {
                       vimp <- varImp(object, scale = FALSE)
                       vimp <- vimp[
                                    order(vimp$Overall, decreasing = TRUE)
                                    ,,drop = FALSE]
                       vimp$var <- rownames(vimp)
                       vimp
                     },
                     selectSize = pickSizeBest,
                     selectVar = pickVars)


rfFuncs <-  list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   library(randomForest)
                   randomForest(x, y, importance = first, ...)
                 },
                 pred = function(object, x)
                 {
                   predict(object, x)
                 },
                 rank = function(object, x, y)
                 {
                   vimp <- varImp(object)

                   if(is.factor(y))
                     {
                       if(all(levels(y) %in% colnames(vimp)))
                         {
                           avImp <- apply(vimp[, levels(y), drop = TRUE],
                                          1,
                                          mean)
                           vimp$Overall <- avImp
                         }

                     }
                   
                   vimp <- vimp[
                                order(
                                      vimp$Overall,
                                      decreasing = TRUE)
                                ,,
                                drop = FALSE]
                   
                   vimp$var <- rownames(vimp)                  
                   vimp
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars)


lmFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...)
                {
                  tmp <- as.data.frame(x)
                  tmp$y <- y
                  lm(y~., data = tmp)
                },
                pred = function(object, x)
                {
                  predict(object, x)
                },
                rank = function(object, x, y)
                {
                  
                  vimp <- varImp(object, scale = FALSE)        
                  vimp <- vimp[
                               order(
                                     vimp$Overall,
                                     decreasing = TRUE)
                               ,,
                               drop = FALSE]
                  vimp$var <- rownames(vimp)
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)


nbFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...)
                {
                  library(klaR)
                  NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
                },
                pred = function(object, x)
                {
                  predict(object, x)$class
                },
                rank = function(object, x, y)
                {
                  vimp <- filterVarImp(x, y)
                  if(is.factor(y))
                    {
                      avImp <- apply(vimp, 1, mean)
                      vimp$Overall <- avImp
                    }
                  
                  vimp <- vimp[
                               order(
                                     vimp$Overall,
                                     decreasing = TRUE)
                               ,,
                               drop = FALSE]
                  
                  vimp$var <- rownames(vimp)                  
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)


######################################################################
######################################################################
## lattice functions


densityplot.rfe <- function(x,
                            data = NULL,
                            metric = x$metric,
                            ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    data$Variable <- factor(data$Variable,
                            levels = paste(sort(unique(data$Variable))))

    form <- as.formula(paste("~", metric, "|Variable"))
    densityplot(form, data = data, ...)
  }

histogram.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    data$Variable <- factor(data$Variable,
                            levels = paste(sort(unique(data$Variable))))

    form <- as.formula(paste("~", metric, "|Variable"))
    histogram(form, data = data, ...)
  }

stripplot.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    data$Variable <- factor(data$Variable,
                            levels = paste(sort(unique(data$Variable))))
    theDots <- list(...)
    if(any(names(theDots) == "horizontal"))
      {
        formText <- if(theDots$horizontal) paste("Variable ~", metric) else paste(metric, "~ Variable")
      } else  formText <- paste("Variable ~", metric)

    form <- as.formula(formText)
    
    stripplot(form, data = data, ...)
    
  }


xyplot.rfe <- function(x,
                       data = NULL,
                       metric = x$metric,
                       ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)

    form <- as.formula(paste(metric, " ~ Variables"))
    xyplot(form, data = data, ...)
  }

######################################################################
######################################################################
## other functions

predictors.rfe <- function(x, ...) x$optVariables

varImp.rfe <- function(object, drop = FALSE, ...)
  {

    sizeIndex <- which(object$results$Variables == object$optsize)
    getImp <- function(u, i) u[[i]]
    imp <- lapply(object$variables, getImp, i = sizeIndex)
    k <- length(imp)
    imp <- do.call("rbind", imp)
    if(drop) imp <- subset(imp, var %in% object$optVar)
    out <- aggregate(imp$Overall, list(var = imp$var), sum, na.rm = TRUE)
    out$x <- out$x/k
    rownames(out) <- out$var
    out$var <- NULL
    names(out) <- "Overall"
    out[order(-out$Overall),,drop = FALSE]

  }


