## test with
## class probs
## seq factors
## mpi
## glmnet failed


progress <- function(x)
  {
    cat("Fitting: ")
    cat(paste(colnames(x), x, sep = "=", collapse = ", "))
    cat("\n")
  }

MeanSD <- function(x, exclude = NULL)
  {
    if(!is.null(exclude)) x <- x[, !(colnames(x) %in% exclude), drop = FALSE]
    out <- c(colMeans(x), sd(x))
    names(out)[-(1:ncol(x))] <- paste(names(out)[-(1:ncol(x))], "SD", sep = "")
    out
  }
    
expandParameters <- function(fixed, seq)
  {
    if(is.null(seq)) return(fixed)

    isSeq <- names(fixed) %in% names(seq)
    out <- fixed
    for(i in 1:nrow(seq))
      {
        tmp <- fixed
        tmp[,isSeq] <- seq[i,]
        out <- rbind(out, tmp)
      }
    out
  }

nominalTrainWorkflow <- function(dat, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    printed <- format(info$loop)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))


    ## For 632 estimator, add an element to the index of zeros to trick it into
    ## fitting and predicting the full data set.

    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)
    
    result <- foreach(iter = seq(along = resampleIndex), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop") %:%
      foreach(parm = 1:nrow(info$loop), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE])

      if(names(resampleIndex)[iter] != "AllData")
        {
          modelIndex <- resampleIndex[[iter]]
          holdoutIndex <- -unique(resampleIndex[[iter]])
        } else {
          modelIndex <- 1:nrow(dat)
          holdoutIndex <- modelIndex
        }
      
      if(testing) cat("pre-model\n")

      mod <- createModel(data = dat[modelIndex,,drop = FALSE ],
                         method = method,
                         tuneValue = info$loop[parm,,drop = FALSE],
                         obsLevels = lev,
                         p = ppp,
                         ...)

      predicted <- predictionFunction(method = method,
                                      modelFit = mod$fit,
                                      newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                      preProc = mod$preProc,
                                      param = info$seqParam[[parm]])

      
      if(testing) print(head(predicted))
      if(ctrl$classProbs)
        {
          probValues <- probFunction(method = method,
                                     modelFit = mod$fit,
                                     newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                     preProc = mod$preProc,
                                     param = info$seqParam[[parm]])
          if(testing) print(head(probValues))
        }

      ##################################

      if(!is.null(info$seq))
        {
          ## collate the predicitons across all the sub-models
          predicted <- lapply(predicted,
                              function(x, y, lv)
                              {
                                if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                                data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                              },
                              y = dat$.outcome[holdoutIndex],
                              lv = lev)
          if(testing) print(head(predicted))

          ## same for the class probabilities
          if(ctrl$classProbs)
            {
              for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
            }
                    
          ## get the performance for this resample for each sub-model
          thisResample <- lapply(predicted,
                                 ctrl$summaryFunction,
                                 lev = lev,
                                 model = method)
          if(testing) print(head(thisResample))
          ## for classification, add the cell counts
          if(length(lev) > 1)
            {
              cells <- lapply(predicted,
                              function(x) flatTable(x$pred, x$obs))
              for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
            }
          thisResample <- do.call("rbind", thisResample)
  
          allParam <- expandParameters(info$loop[parm,,drop = FALSE], info$seqParam[[parm]])
          
          ## For ctree, we had to repeat the first value
          if(method == "ctree") allParam <- allParam[!duplicated(allParam),, drop = FALSE]

          ## For glmnet, we fit all the lambdas but x$fixed has an NA
          if(method == "glmnet") allParam <- allParam[complete.cases(allParam),, drop = FALSE]
          
          thisResample <- cbind(allParam, thisResample)
 
        } else {
          
          if(is.factor(dat$.outcome)) predicted <- factor(as.character(predicted),
                                                          levels = lev)
          tmp <-  data.frame(pred = predicted,
                             obs = dat$.outcome[holdoutIndex],
                             stringsAsFactors = FALSE)
          if(ctrl$classProbs) tmp <- cbind(tmp, probValues)


          ##################################
          thisResample <- ctrl$summaryFunction(tmp,
                                               lev = lev,
                                               model = method)

          ## if classification, get the confusion matrix
          if(length(lev) > 1) thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
          
          thisResample <- as.data.frame(t(thisResample))
          thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])

        }
      thisResample$Resample <- names(resampleIndex)[iter]
      thisResample
    }


   if(ctrl$method %in% c("boot632"))
      {
        perfNames <- names(ctrl$summaryFunction(data.frame(obs = dat$.outcome, pred = sample(dat$.outcome)),
                                                lev = lev,
                                                model = method))
        apparent <- subset(result, Resample == "AllData")
        apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
        names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                        "Apparent", sep = "")
        names(apparent) <- gsub("^\\.", "", names(apparent))
        result <- subset(result, Resample != "AllData")
      }
    names(result) <- gsub("^\\.", "", names(result))
 
    out <- ddply(result[,!grepl("^cell|Resample", colnames(result)),drop = FALSE],
                 info$model$parameter,
                 MeanSD, exclude = info$model$parameter)
    
    if(ctrl$method %in% c("boot632"))
      {
        out <- merge(out, apparent)
       for(p in seq(along = perfNames))
        {
          const <- 1-exp(-1)
          out[, perfNames[p]] <- (const * out[, perfNames[p]]) +  ((1-const) * out[, paste(perfNames[p],"Apparent", sep = "")])
        }
      }
    
    list(performance = out, resamples = result)
  }


looTrainWorkflow <- function(dat, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    printed <- format(info$loop)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))
    
    result <- foreach(iter = seq(along = ctrl$index), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop") %:%
      foreach(parm = 1:nrow(info$loop), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE])

      if(testing) cat("pre-model\n")
      
      mod <- createModel(data = dat[ctrl$index[[iter]],,drop = FALSE ],
                         method = method,
                         tuneValue = info$loop[parm,,drop = FALSE],
                         obsLevels = lev,
                         p = ppp,
                         ...)
      
      holdoutIndex <- -unique(ctrl$index[[iter]])

      predicted <- predictionFunction(method = method,
                                      modelFit = mod$fit,
                                      newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                      preProc = mod$preProc,
                                      param = info$seqParam[[parm]])
      
      if(testing) print(head(predicted))
      if(ctrl$classProbs)
        {
          probValues <- probFunction(method = method,
                                     modelFit = mod$fit,
                                     newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                     preProc = mod$preProc,
                                     param = info$seqParam[[parm]])
          if(testing) print(head(probValues))
        }

      ##################################

      if(!is.null(info$seq))
        {
          ## collate the predictions across all the sub-models
          predicted <- lapply(predicted,
                              function(x, y, lv)
                              {
                                if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                                data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                              },
                              y = dat$.outcome[holdoutIndex],
                              lv = lev)
          if(testing) print(head(predicted))

          ## same for the class probabilities
          if(ctrl$classProbs)
            {
              for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
            }
          predicted <- do.call("rbind", predicted)
          allParam <- expandParameters(info$loop, info$seqParam[[parm]])
          predicted <- cbind(predicted, allParam)
        } else {
          
          if(is.factor(dat$.outcome)) predicted <- factor(as.character(predicted),
                                                          levels = lev)
          predicted <-  data.frame(pred = predicted,
                                   obs = dat$.outcome[holdoutIndex],
                                   stringsAsFactors = FALSE)
          if(ctrl$classProbs) predicted <- cbind(predicted, probValues)
          predicted <- cbind(predicted, info$loop[parm,,drop = FALSE])
        }
      predicted
    }

    names(result) <- gsub("^\\.", "", names(result))
    out <- ddply(result,
                 info$model$parameter,
                 ctrl$summaryFunction,
                 lev = lev,
                 model = method)
    out
  }


oobTrainWorkflow <- function(dat, info, method, ppOpts, ctrl, lev, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    printed <- format(info$loop)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))    
    result <- foreach(parm = 1:nrow(info$loop), .combine = "rbind") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE])

      mod <- createModel(data = dat,
                         method = method,
                         tuneValue = info$loop[parm,,drop = FALSE],
                         obsLevels = lev,
                         p = ppp,
                         ...)
      out <- switch(
                    class(mod$fit)[1],
                    randomForest = rfStats(mod$fit),
                    RandomForest = cforestStats(mod$fit),
                    bagEarth =, bagFDA = bagEarthStats(mod$fit),
                    regbagg =, classbagg = ipredStats(mod$fit))
      cbind(as.data.frame(t(out)), info$loop[parm,,drop = FALSE])
    }
    names(result) <- gsub("^\\.", "", names(result))
    result
  }


################################################################################################

nominalSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)

    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])
      
      sbfResults <- sbfIter(x[modelIndex,,drop = FALSE],
                            y[modelIndex],
                            x[holdoutIndex,,drop = FALSE],
                            y[holdoutIndex],
                            ctrl,
                            ...)
      resamples <- ctrl$functions$summary(sbfResults$pred, lev = lev)
      if(is.factor(y)) resamples <- c(resamples, flatTable(sbfResults$pred$pred, sbfResults$pred$obs))
      resamples <- data.frame(t(resamples))
      resamples$Resample <- names(resampleIndex)[iter]
      
      list(resamples = resamples, selectedVars = sbfResults$variables)
    }

    resamples <- do.call("rbind", result[names(result) == "resamples"])
    performance <- MeanSD(resamples[,!grepl("Resample", colnames(resamples)),drop = FALSE])
    
    if(ctrl$method %in% c("boot632"))
      {
        modelIndex <- 1:nrow(x)
        holdoutIndex <- modelIndex
        appResults <- sbfIter(x[modelIndex,,drop = FALSE],
                              y[modelIndex],
                              x[holdoutIndex,,drop = FALSE],
                              y[holdoutIndex],
                              ctrl,
                              ...)
        apparent <- ctrl$functions$summary(appResults$pred, lev = lev)
        perfNames <- names(apparent)

        const <- 1-exp(-1)

        for(p in seq(along = perfNames))
          performance[perfNames[p]] <- (const * performance[perfNames[p]]) +  ((1-const) * apparent[perfNames[p]])
        
      }
    
    list(performance = performance, everything = result)
  }


looSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index

    vars <- vector(mode = "list", length = length(y))
    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])
      
      sbfResults <- sbfIter(x[modelIndex,,drop = FALSE],
                            y[modelIndex],
                            x[holdoutIndex,,drop = FALSE],
                            y[holdoutIndex],
                            ctrl,
                            ...)

      sbfResults
    }
    resamples <- do.call("rbind", result[names(result) == "pred"])
    performance <- ctrl$functions$summary(resamples, lev = lev)
    
    list(performance = performance, everything = result)
  }


################################################################################################

nominalRfeWorkflow <- function(x, y, sizes, ppOpts, ctrl, lev, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)

    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)

      if(names(resampleIndex)[iter] != "AllData")
        {
          modelIndex <- resampleIndex[[iter]]
          holdoutIndex <- -unique(resampleIndex[[iter]])
        } else {
          modelIndex <- 1:nrow(x)
          holdoutIndex <- modelIndex
        }
      
      rfeResults <- rfeIter(x[modelIndex,,drop = FALSE],
                            y[modelIndex],
                            x[holdoutIndex,,drop = FALSE],
                            y[holdoutIndex],
                            sizes,
                            ctrl,
                            ...)
      resamples <- ddply(rfeResults$pred, .(Variables), ctrl$functions$summary, lev = lev)
      if(is.factor(y))
        {
          cells <- ddply(rfeResults$pred, .(Variables), function(x) flatTable(x$pred, x$obs))
          resamples <- merge(resamples, cells)
        }
          
      resamples$Resample <- names(resampleIndex)[iter]
      vars <- do.call("rbind", rfeResults$finalVariables)
      vars$Resample <- names(resampleIndex)[iter]
      list(resamples = resamples, selectedVars = vars)
    }
    resamples <- do.call("rbind", result[names(result) == "resamples"])
    rownames(resamples) <- NULL

    if(ctrl$method %in% c("boot632"))
      {
        perfNames <- names(ctrl$functions$summary(data.frame(obs =y, pred = sample(y)),
                                                  lev = lev,
                                                  model = method))
        apparent <- subset(resamples, Resample == "AllData")
        apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
        names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                        "Apparent", sep = "")
        names(apparent) <- gsub("^\\.", "", names(apparent))
        resamples <- subset(resamples, Resample != "AllData")
      }
    
    externPerf <- ddply(resamples[,!grepl("\\.cell|Resample", colnames(resamples)),drop = FALSE],
                        .(Variables),
                        MeanSD,
                        exclude = "Variables")
    if(ctrl$method %in% c("boot632"))
      {
        externPerf <- merge(externPerf, apparent)
        for(p in seq(along = perfNames))
          {
            const <- 1-exp(-1)
            externPerf[, perfNames[p]] <- (const * externPerf[, perfNames[p]]) +  ((1-const) * externPerf[, paste(perfNames[p],"Apparent", sep = "")])
          }
        externPerf <- externPerf[, !(names(externPerf) %in% paste(perfNames,"Apparent", sep = ""))]
      }
    list(performance = externPerf, everything = result)
  }


looRfeWorkflow <- function(x, y, sizes, ppOpts, ctrl, lev, ...)
  {
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %dopar%
    {
      library(caret)

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])
      
      rfeResults <- rfeIter(x[modelIndex,,drop = FALSE],
                            y[modelIndex],
                            x[holdoutIndex,,drop = FALSE],
                            y[holdoutIndex],
                            sizes,
                            ctrl,
                            ...)
      rfeResults
    }
    preds <- do.call("rbind", result[names(result) == "pred"])
    resamples <- ddply(preds, .(Variables), ctrl$functions$summary, lev = lev)   
    list(performance = resamples, everything = result)
  }


