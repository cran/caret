## need:
##     - predict for rfe and sbf

sbfIter <- function(x, y,
                    testX, testY, 
                    sbfControl = sbfControl(), ...)
{
  if(is.null(colnames(x))) stop("x must have column names")

  if(is.null(testX) | is.null(testY)) stop("a test set must be specified")

  scores <- apply(x, 2, sbfControl$functions$score, y = y)

  retained <- sbfControl$functions$filter(scores, x, y)  
  ## deal with zero length results
  
  testX <- testX[, which(retained), drop = FALSE]
  
  fitObject <- sbfControl$functions$fit(x[, which(retained), drop = FALSE],
                                        y,
                                        ...)  

  modelPred <- data.frame(pred = sbfControl$functions$pred(fitObject, testX),
                          obs = testY)
  
  list(variables = names(retained)[which(retained)],
       pred = modelPred)

}

######################################################################
######################################################################

## This function will be executed to do the selection by filter iterations over
## different resampling iterations.

## The input is a single list that has elelemnts for:
##
## - indices:  a list of indices for which samples are in the training
##             set. This can include more than one resampling iteration
## - x: the full data set of predictors
## - y: the full set of outcomes
## - cntl: output from sbfControl

sbfWrapper <- function(X)
  {
    ## iterate over the resampling iterations
    index <- X$index
    X$index <- NULL
    out <- vector(mode = "list", length = length(index))
    for(i in seq(along = index))
      {
        out[[i]] <- do.call("sbfChunk",
                            c(list(inTrain= index[[i]]), X))
      }
    out
  }

######################################################################
######################################################################

sbfChunk <- function(inTrain, x, y, cntl, sizes, ...)
  {

    findMatch <- function(x, y)
      {
        if(length(x) != length(y)) return(FALSE)
        all(x == y)
      }
    
    j <- which(unlist(lapply(cntl$index, findMatch, y = inTrain)))
    
    if(length(j) == 0) stop("can't figure out which resample iteration this is")

    if(cntl$verbose) cat("\nExternal resampling iter:\t", j)
    flush.console()
    
    inTrainX <- x[inTrain, ]
    outTrainX <- x[-inTrain, ]
    inTrainY <- y[inTrain]
    outTrainY <- y[-inTrain]      
    sbfResults <- sbfIter(inTrainX, inTrainY,
                          outTrainX, outTrainY,
                          cntl,
                          ...)
    
    sbfResults$pred$resampleIter <- rep(j, nrow(sbfResults$pred))

    out <- list(pred = sbfResults$pred,
                selectedVars = sbfResults$variables)
    out
  }

######################################################################
######################################################################

sbf <- function (x, ...) UseMethod("sbf")

"sbf.default" <-
  function(x, y,
           sbfControl = sbfControl(), ...)
{
  funcCall <- match.call(expand.dots = TRUE)

  numFeat <- ncol(x)

  if(is.null(sbfControl$index))
    sbfControl$index <- switch(tolower(sbfControl$method),
                               cv = createFolds(y, sbfControl$number, returnTrain = TRUE),
                               loocv = createFolds(y, length(y), returnTrain = TRUE),
                               boot = createResample(y, sbfControl$number),
                               test = createDataPartition(y, 1, sbfControl$p),
                               lgocv = createDataPartition(y, sbfControl$number, sbfControl$p))
  
  ## check summary function and metric
  test <- sbfControl$functions$summary(data.frame(obs = y, pred = sample(y)))
  perfNames <- names(test)

  ## need to setup lists for each worker (!= each task)

  tmp <- repList(
                 list(x = x,
                      y = y,
                      cntl = sbfControl,
                      ...),
                 sbfControl$workers) ## define this var

  indexSplit <- splitIndicies(length(sbfControl$index),
                              sbfControl$workers)
  for(i in seq(along = tmp)) tmp[[i]]$index <- sbfControl$index[indexSplit == i]
  
  ## Now, we setup arguments to lapply (or similar functions) executed via do.call
  ## workerData will split up the data need for the jobs
  argList <- list(X = tmp,
                  FUN = sbfWrapper)

  ## Append the extra objects needed to do the work (See the parallel examples in
  ## ?train to see examples
  if(!is.null(sbfControl$computeArgs)) argList <- c(argList, sbfControl$computeArgs)
 
  sbfResults <- do.call(sbfControl$computeFunction, argList)
  
  sbfPred <- lapply(sbfResults,
                    function(x) lapply(x,
                                       function(y) y$pred))[[1]]
  sbfPred <- do.call("rbind", sbfPred)

  sbfPred <- split(sbfPred, sbfPred$resampleIter)
  resamples <- lapply(sbfPred, sbfControl$functions$summary)
  resamples <- as.data.frame(do.call("rbind", resamples))

  externPerf <- cbind(t(apply(resamples, 2, mean, na.rm = TRUE)),
                      t(apply(resamples, 2, sd, na.rm = TRUE)))
  colnames(externPerf)[3:4] <- paste(colnames(externPerf)[3:4], "SD", sep = "")
 
  #########################################################################

  selectedVars <- lapply(sbfResults,
                    function(x) lapply(x,
                                       function(y) y$selectedVars))
  varList <- unique(unlist(selectedVars))

  scores <- apply(x, 2, sbfControl$functions$score, y = y)
  retained <- sbfControl$functions$filter(scores, x, y)
  
  fit <- sbfControl$functions$fit(x[, retained, drop = FALSE],
                                  y,
                                  ...)

  resamples <- switch(sbfControl$returnResamp,
                      none = NULL, 
                      all = resamples)

  #########################################################################
  ## Now, based on probability or static ranking, figure out the best vars
  ## and the best subset size and fit final model
  
  structure(
            list(
                 pred = if(sbfControl$saveDetails) sbfPred else NULL,
                 variables = selectedVars,
                 results = as.data.frame(externPerf),
                 fit = fit,
                 optVariables = names(retained)[retained],
                 call = funcCall,
                 control = sbfControl,
                 resample = resamples,
                 metrics = perfNames,
                 dots = list(...)),
            class = "sbf")
}

sbf.formula <- function (form, data, ..., subset, na.action, contrasts = NULL) 
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
  res <- sbf(as.data.frame(x), y, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("sbf", "sbf.formula")
  res
}

######################################################################
######################################################################

print.sbf <- function(x, top = 5, digits = max(3, getOption("digits") - 3), ...)
{

  cat("\nSelection By Filter\n\n")

  cat("Outer resamping method was",
      x$control$number,
      "iterations of",
      switch(x$control$method,
             "boot" = "the bootstrap.",
             "LGOCV" = "leave group out cross-validation.",
             "LOOCV" = "leave one out cross-validation.",
             "cv" = "cross-validation."),
      "\n")

  cat("\nResampling performance:\n\n")
  print(format(x$results, digits = digits), row.names = FALSE)
  cat("\n")

  if(length(x$optVariables) > 0)
    {
      cat("Using the training set, ",
          length(x$optVariables),
          ifelse(length(x$optVariables) > 1,
                 " variables were selected:\n   ",
                 " variable was selected:\n   "),
          paste(x$optVariables[1:min(top, length(x$optVariables))],
                collapse = ", "),
           ifelse(length(x$optVariables) > top, "..", ""),
          ".\n\n",
          sep = "")
    } else cat("No variables were selected from the training set.\n\n")
  
 
  vars <- sort(table(unlist(x$variables)), decreasing = TRUE)

  top <- min(top, length(vars))
  
  smallVars <- vars[1:top]
  smallVars <- round(smallVars/x$control$number*100, 1)

  varText <- paste(names(smallVars), " (",
                   smallVars, "%)", sep = "")
  varText <- paste(varText, collapse = ", ")

  if(!all(is.na(smallVars)))
    {
      cat("During resampling, the top ",
          top,
          " selected variables (out of a possible ",
          length(vars),
          "):\n   ",
          varText,
          "\n\n",
          sep = "")
      cat("On average, ",
          round(mean(unlist(lapply(x$variables[[1]], length))), 1),
          " variables were selected (min = ",
          round(min(unlist(lapply(x$variables[[1]], length))), 1),
          ", max = ",
          round(max(unlist(lapply(x$variables[[1]], length))), 1),
          ")\n",
          sep = "")
    } else {
      cat("During resampling, no variables were selected.\n\n")
    }
  

  
  invisible(x)
}

######################################################################
######################################################################

sbfControl <- function(functions = NULL,
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
       functions = if(is.null(functions)) caretSBF else functions,
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

anovaScores <- function(x, y)
  {
    pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], silent = TRUE)
    if(any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) pv <- 1
    pv
  }

gamScores <- function(x, y)
  {
    library(gam)
    pv <- try(anova(gam(y ~ s(x)), test = "F")[2, "Pr(F)"], silent = TRUE)
    if(any(class(pv) == "try-error")) pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], silent = TRUE)
    if(any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) pv <- 1
    pv
  }

caretSBF <- list(summary = defaultSummary,
                 fit = function(x, y, ...)
                 {
                   if(ncol(x) > 0)
                     {
                       train(x, y, ...)
                     } else nullModel(y = y)                                      
                 },
                 pred = function(object, x)
                 {
                   if(class(object) != "nullModel")
                     {
                       modelPred <- extractPrediction(
                                                      list(object),
                                                      unkX = x,
                                                      unkOnly = TRUE)
                       modelPred <- modelPred[modelPred$dataType == "Unknown",]
                       modelPred$pred
                     } else predict(object, x) 
                 },
                 score = function(x, y)
                 {
                   ## should return a named logical vector
                   if(is.factor(y)) anovaScores(x, y) else gamScores(x, y)
                 },
                 filter = function(score, x, y) score <= 0.05
                 )

rfSBF <- list(summary = defaultSummary,
              fit = function(x, y, ...)
              {
                if(ncol(x) > 0)
                  {
                    library(randomForest)
                    randomForest(x, y, ...)
                  } else nullModel(y = y)
              },
              pred = function(object, x)
              {
                predict(object, x)
              },
              score = function(x, y)
              {
                ## should return a named logical vector
                if(is.factor(y)) anovaScores(x, y) else gamScores(x, y)
              },
              filter = function(score, x, y) score <= 0.05
              )

lmSBF <- list(summary = defaultSummary,
              fit = function(x, y, ...)
              {
                if(ncol(x) > 0)
                  {
                    tmp <- as.data.frame(x)
                    tmp$y <- y
                    lm(y~., data = tmp)
                  } else nullModel(y = y)                
              },
              pred = function(object, x)
              {
                predict(object, x)
              },
              score = function(x, y)
              {
                anovaScores(y, x)
              },
              filter = function(score, x, y) score <= 0.05
              )

ldaSBF <- list(summary = defaultSummary,
               fit = function(x, y, ...)
               {
                 if(ncol(x) > 0)
                   {
                     library(MASS)
                     lda(x, y, ...)
                   } else nullModel(y = y)
               },
               pred = function(object, x)
               {
                 if(class(object) == "nullModel") predict(object, x) else predict(object, x)$class
               },
               score = function(x, y)
               {
                 ## should return a named logical vector
                 anovaScores(x, y)
               },
               filter = function(score, x, y) score <= 0.05
               )

nbSBF <- list(summary = defaultSummary,
              fit = function(x, y, ...)
              {
                if(ncol(x) > 0)
                  {
                    library(klaR)
                    NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)

                  } else nullModel(y = y)
              },
              pred = function(object, x)
              {
                if(class(object) == "nullModel") predict(object, x) else predict(object, x)$class
              },              

              pred = function(object, x)
              {
                predict(object, x)$class
              },
              score = function(x, y)
              {
                ## should return a named logical vector
                anovaFilter(x, y)
              },
                 filter = function(score, x, y) score <= 0.05
              )



treebagSBF <- list(summary = defaultSummary,
                     fit = function(x, y, ...)
                     {
                       if(ncol(x) > 0)
                         {
                           library(ipred)
                           ipredbagg(y, x, ...)
                         } else nullModel(y = y)
                     },

                     pred = function(object, x)
                     {
                       predict(object, x)
                     },
                     score = function(x, y)
                     {
                       ## should return a named logical vector
                       anovaScores(x, y)
                     },
                   filter = function(score, x, y) score <= 0.05
                   )




######################################################################
######################################################################
## lattice functions


densityplot.sbf <- function(x,
                            data = NULL,
                            metric = x$metric[1],
                            ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    form <- as.formula(paste("~", metric))
    densityplot(form, data = data, ...)
  }

histogram.sbf <- function(x,
                          data = NULL,
                          metric = x$metric[1],
                          ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)

    form <- as.formula(paste("~", metric))
    histogram(form, data = data, ...)
  }




######################################################################
######################################################################
## other functions

predictors.sbf <- function(x, ...) x$optVariables

varImp.sbf <- function(object, onlyFinal = TRUE, ...)
  {

    vars <- sort(table(unlist(object$variables)), decreasing = TRUE)/object$control$number
    
    
    out <- as.data.frame(vars)
      names(out) <- "Overall"
    if(onlyFinal) out <- subset(out, rownames(out) %in% object$optVariables)
    out[order(-out$Overall),,drop = FALSE]

  }

######################################################################
## what to do when no predictors are selected?

nullModel <- function (x, ...) UseMethod("nullModel")

nullModel.default <- function(x = NULL, y, ...)
  {

    if(is.factor(y))
      {
        lvls <- levels(y)
        tab <- table(y)
        value <- names(tab)[which.max(tab)]
      } else {
        lvls <- NULL
        value <- mean(y, na.rm = TRUE)
      }
    structure(
              list(call = match.call(),
                   value = value,
                   levels = lvls,
                   n = length(y)),
              class = "nullModel")
  }

print.nullModel <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("Null",
      ifelse(is.null(x$levels), "Classification", "Regression"),
      "Model\n")
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  cat("Predicted Value:",
      ifelse(is.null(x$levels), format(x$value, digitis = digits), x$value),
      "\n")
}

predict.nullModel <- function (object, newdata = NULL, ...)
  {
    n <- if(is.null(newdata)) object$n else nrow(newdata)
    if(!is.null(object$levels))
      {
        out <- factor(rep(object$value, n), levels = object$levels)
      } else out <- rep(object$value, n)

    out
  }


  


if(FALSE)
  {
    data(BloodBrain)


    set.seed(1)
    RFwithGAM <- sbf(bbbDescr, logBBB,
                      sbfControl = sbfControl(functions = rfSBF,
                        verbose = FALSE))

    test <- rfSBF
    test$filter <-  function(score, x, y)
      {
        ## FDR of 5%
        out <- bhAdjust(score) <= 0.05 
        out
      }
    

        set.seed(1)
    RFwithGAM2 <- sbf(bbbDescr, logBBB,
                      sbfControl = sbfControl(functions = test,
                        verbose = FALSE))
    
    
    test <- rfSBF
    test$filter <-  function(score, x, y)
      {
        ## Don't keep if no information
        hasVar <- apply(x, 2, function(x) length(unique(x)) > 1)
        corX <- cor(x[, hasVar, drop = FALSE])

        ## Find minimal set with pair-wide correlations < 0.75
        tooHigh <- findCorrelation(corX, .75)
        if(length(tooHigh) > 1) hasVar[tooHigh] <- FALSE

        ## FDR of 5%
        out <- bhAdjust(score) <= 0.05 & hasVar
        out
      }
    
    set.seed(1)
    RFwithGAM3 <- sbf(bbbDescr, logBBB,
                      sbfControl = sbfControl(functions = test,
                        verbose = FALSE))
    RFwithGAM
    RFwithGAM2
    RFwithGAM3
    

  }
