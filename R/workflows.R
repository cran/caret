### In this file, there are a lot of functions form caret that are
### references using the explicit namespace operator (:::). For some
### reason, with some parallel processing technologies and foreach,
### functions inside of caret cannot be found despite using the
### ".packages" argument and calling the caret package via library().


progress <- function(x, names, iter, start = TRUE)
  {
    text <- paste(ifelse(start, "+ ", "- "),
                  names[iter], ": ",
                  paste(colnames(x), x, sep = "=", collapse = ", "),
                  sep = "")
    cat(text, "\n")
  }

MeanSD <- function(x, exclude = NULL)
  {
    if(!is.null(exclude)) x <- x[, !(colnames(x) %in% exclude), drop = FALSE]
    out <- c(colMeans(x, na.rm = TRUE), sapply(x, sd, na.rm = TRUE))
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
    suppressPackageStartupMessages(library(foreach))
    library(caret)
    loadNamespace("caret")
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
   
    printed <- format(info$loop, digits = 4)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))


    ## For 632 estimator, add an element to the index of zeros to trick it into
    ## fitting and predicting the full data set.

    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)

    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %:%
      foreach(parm = 1:nrow(info$loop), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                            names(resampleIndex), iter)

      if(names(resampleIndex)[iter] != "AllData")
        {
          modelIndex <- resampleIndex[[iter]]
          holdoutIndex <- -unique(resampleIndex[[iter]])
        } else {
          modelIndex <- 1:nrow(dat)
          holdoutIndex <- modelIndex
        }
      
      if(testing) cat("pre-model\n")

      mod <- try(
                 caret:::createModel(data = dat[modelIndex,,drop = FALSE ],
                                     method = method,
                                     tuneValue = info$loop[parm,,drop = FALSE],
                                     obsLevels = lev,
                                     pp = ppp,
                                     custom = ctrl$custom$model,
                                     ...),
                 silent = TRUE)

      if(class(mod)[1] != "try-error")
        {
          predicted <- try(
                           caret:::predictionFunction(method = method,
                                                      modelFit = mod$fit,
                                                      newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                                      preProc = mod$preProc,
                                                      param = info$seqParam[[parm]],
                                                      custom = ctrl$custom$prediction),
                           silent = TRUE)

          if(class(predicted)[1] == "try-error")
            {
              wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                           printed[parm,,drop = FALSE],
                           sep = "=",
                           collapse = ", ")
              wrn <- paste("predictions failed for ", names(resampleIndex)[iter], ": ", wrn, sep = "")
              if(ctrl$verboseIter) cat(wrn, "\n")
              warning(wrn)
              rm(wrn)
              
              ## setup a dummy results with NA values for all predictions
              nPred <- nrow(dat) - length(unique(holdoutIndex))
              if(!is.null(lev))
                {
                  predicted <- rep("", nPred)
                  predicted[seq(along = predicted)] <- NA
                } else {
                  predicted <- rep(NA, nPred)
                }
              if(!is.null(info$seqParam[[parm]]))
                {
                  tmp <- predicted
                  predicted <- vector(mode = "list", length = nrow(info$seqParam[[parm]]) + 1)
                  for(i in seq(along = predicted)) predicted[[i]] <- tmp
                  rm(tmp)
                }
            }
        } else {
          wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                       printed[parm,,drop = FALSE],
                       sep = "=",
                       collapse = ", ")
          wrn <- paste("model fit failed for ", names(resampleIndex)[iter], ": ", wrn, sep = "")
          if(ctrl$verboseIter) cat(wrn, "\n")
          warning(wrn)
          rm(wrn)
        
          ## setup a dummy results with NA values for all predictions
          nPred <- nrow(dat) - length(unique(holdoutIndex))
          if(!is.null(lev))
            {
              predicted <- rep("", nPred)
              predicted[seq(along = predicted)] <- NA
            } else {
              predicted <- rep(NA, nPred)
            }
          if(!is.null(info$seqParam[[parm]]))
            {
              tmp <- predicted
              predicted <- vector(mode = "list", length = nrow(info$seqParam[[parm]]) + 1)
              for(i in seq(along = predicted)) predicted[[i]] <- tmp
              rm(tmp)
            }
        }
     
      if(testing) print(head(predicted))
      if(ctrl$classProbs)
        {
          if(class(mod)[1] != "try-error")
            {
              probValues <- caret:::probFunction(method = method,
                                                 modelFit = mod$fit,
                                                 newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                                 preProc = mod$preProc,
                                                 param = info$seqParam[[parm]],
                                                 custom = ctrl$custom$probability)
            } else {
              probValues <- as.data.frame(matrix(NA, nrow = nPred, ncol = length(lev)))
              colnames(probValues) <- lev
              if(!is.null(info$seqParam[[parm]]))
                {
                  tmp <- probValues
                  probValues <- vector(mode = "list", length = nrow(info$seqParam[[parm]]) + 1)
                  for(i in seq(along = probValues)) probValues[[i]] <- tmp
                  rm(tmp)
                }
            }
          if(testing) print(head(probValues))
        }

      ##################################

      if(!is.null(info$seq))
        {
          ## merge the fixed and seq parameter values together
          allParam <- caret:::expandParameters(info$loop[parm,,drop = FALSE], info$seqParam[[parm]])
          
          ## For ctree, we had to repeat the first value
          if(method == "ctree") allParam <- allParam[!duplicated(allParam),, drop = FALSE]

          ## For glmnet, we fit all the lambdas but x$fixed has an NA
          if(method == "glmnet") allParam <- allParam[complete.cases(allParam),, drop = FALSE]
          
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

          if(ctrl$savePredictions)
            {
              tmpPred <- predicted
              for(modIndex in seq(along = tmpPred))
                {
                  tmpPred[[modIndex]]$rowIndex <- (1:nrow(dat))[unique(holdoutIndex)]
                  tmpPred[[modIndex]] <- cbind(tmpPred[[modIndex]], allParam[modIndex,,drop = FALSE])
                }
              tmpPred <- rbind.fill(tmpPred)
              tmpPred$Resample <- names(resampleIndex)[iter]
            } else tmpPred <- NULL
          
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
                              function(x) caret:::flatTable(x$pred, x$obs))
              for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
            }
          thisResample <- do.call("rbind", thisResample)          
          thisResample <- cbind(allParam, thisResample)
 
        } else {       
          if(is.factor(dat$.outcome)) predicted <- factor(as.character(predicted),
                                                          levels = lev)
          tmp <-  data.frame(pred = predicted,
                             obs = dat$.outcome[holdoutIndex],
                             stringsAsFactors = FALSE)
          ## Sometimes the code above does not coerce the first
          ## columnn to be named "pred" so force it
          names(tmp)[1] <- "pred"
          if(ctrl$classProbs) tmp <- cbind(tmp, probValues)

          if(ctrl$savePredictions)
            {
              tmpPred <- tmp
              tmpPred$rowIndex <- (1:nrow(dat))[unique(holdoutIndex)]
              tmpPred <- cbind(tmpPred, info$loop[parm,,drop = FALSE])
              tmpPred$Resample <- names(resampleIndex)[iter]
            } else tmpPred <- NULL

          ##################################
          thisResample <- ctrl$summaryFunction(tmp,
                                               lev = lev,
                                               model = method)

          ## if classification, get the confusion matrix
          if(length(lev) > 1) thisResample <- c(thisResample, caret:::flatTable(tmp$pred, tmp$obs))
          thisResample <- as.data.frame(t(thisResample))
          thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])

        }
      thisResample$Resample <- names(resampleIndex)[iter]

      if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE],
                                            names(resampleIndex), iter, FALSE)
      list(resamples = thisResample, pred = tmpPred)
    }
    
    resamples <- rbind.fill(result[names(result) == "resamples"])
    pred <- if(ctrl$savePredictions)  rbind.fill(result[names(result) == "pred"]) else NULL
    if(ctrl$method %in% c("boot632"))
      {
        perfNames <- names(ctrl$summaryFunction(data.frame(obs = dat$.outcome, pred = sample(dat$.outcome)),
                                                lev = lev,
                                                model = method))
        apparent <- subset(resamples, Resample == "AllData")
        apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
        names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                        "Apparent", sep = "")
        names(apparent) <- gsub("^\\.", "", names(apparent))
        if(any(!complete.cases(apparent[,!grepl("^cell|Resample", colnames(apparent)),drop = FALSE])))
          {
            warning("There were missing values in the apparent performance measures.")
          }        
        resamples <- subset(resamples, Resample != "AllData")
      }
    names(resamples) <- gsub("^\\.", "", names(resamples))

    if(any(!complete.cases(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE])))
      {
        warning("There were missing values in resampled performance measures.")
      }
    out <- ddply(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE],
                 info$model$parameter,
                 caret:::MeanSD, exclude = info$model$parameter)
    
    if(ctrl$method %in% c("boot632"))
      {
        out <- merge(out, apparent)
       for(p in seq(along = perfNames))
        {
          const <- 1-exp(-1)
          out[, perfNames[p]] <- (const * out[, perfNames[p]]) +  ((1-const) * out[, paste(perfNames[p],"Apparent", sep = "")])
        }
      }
    
    list(performance = out, resamples = resamples, predictions = if(ctrl$savePredictions) pred else NULL)
  }


looTrainWorkflow <- function(dat, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
  {
    library(caret)
    
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    printed <- format(info$loop)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))
    
    result <- foreach(iter = seq(along = ctrl$index), .combine = "rbind", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %:%
      foreach(parm = 1:nrow(info$loop), .combine = "rbind", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE])

      if(testing) cat("pre-model\n")
      
      mod <- caret:::createModel(data = dat[ctrl$index[[iter]],,drop = FALSE ],
                                 method = method,
                                 tuneValue = info$loop[parm,,drop = FALSE],
                                 obsLevels = lev,
                                 pp = ppp,
                                 custom = ctrl$custom$model,
                                 ...)
      
      holdoutIndex <- -unique(ctrl$index[[iter]])

      predicted <- caret:::predictionFunction(method = method,
                                              modelFit = mod$fit,
                                              newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                              preProc = mod$preProc,
                                              param = info$seqParam[[parm]],
                                              custom = ctrl$custom$prediction)
      
      if(testing) print(head(predicted))
      if(ctrl$classProbs)
        {
          probValues <- caret:::probFunction(method = method,
                                             modelFit = mod$fit,
                                             newdata = dat[holdoutIndex, !(names(dat) %in% c(".outcome", ".modelWeights")), drop = FALSE],
                                             preProc = mod$preProc,
                                             param = info$seqParam[[parm]],
                                             custom = ctrl$custom$probability)
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
          allParam <- caret:::expandParameters(info$loop, info$seqParam[[parm]])
          predicted <- cbind(predicted, allParam)
          ## if saveDetails then save and export 'predicted'
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
    list(performance = out, predictions = result)
  }


oobTrainWorkflow <- function(dat, info, method, ppOpts, ctrl, lev, ...)
  {
    library(caret)
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    printed <- format(info$loop)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))    
    result <- foreach(parm = 1:nrow(info$loop), .packages = "caret", .combine = "rbind") %dopar%
    {
      library(caret)
      if(ctrl$verboseIter) caret:::progress(printed[parm,,drop = FALSE])

      mod <- caret:::createModel(data = dat,
                                 method = method,
                                 tuneValue = info$loop[parm,,drop = FALSE],
                                 obsLevels = lev,
                                 pp = ppp,
                                 custom = ctrl$custom$model,
                                 ...)
      out <- switch(
                    class(mod$fit)[1],
                    randomForest = caret:::rfStats(mod$fit),
                    RandomForest = caret:::cforestStats(mod$fit),
                    bagEarth =, bagFDA = caret:::bagEarthStats(mod$fit),
                    regbagg =, classbagg = caret:::ipredStats(mod$fit))
      cbind(as.data.frame(t(out)), info$loop[parm,,drop = FALSE])
    }
    names(result) <- gsub("^\\.", "", names(result))
    result
  }


################################################################################################

nominalSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...)
  {
    library(caret)
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)

    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
    {
      library(caret)

      if(names(resampleIndex)[iter] != "AllData")
        {
          modelIndex <- resampleIndex[[iter]]
          holdoutIndex <- -unique(resampleIndex[[iter]])
        } else {
          modelIndex <- 1:nrow(dat)
          holdoutIndex <- modelIndex
        }
      
      sbfResults <- caret:::sbfIter(x[modelIndex,,drop = FALSE],
                                    y[modelIndex],
                                    x[holdoutIndex,,drop = FALSE],
                                    y[holdoutIndex],
                                    ctrl,
                                    ...)
      if(ctrl$saveDetails)
        {
          tmpPred <- sbfResults$pred
          tmpPred$Resample <- names(resampleIndex)[iter]
          tmpPred$rowIndex <- seq(along = y)[unique(holdoutIndex)]
        } else tmpPred <- NULL
      resamples <- ctrl$functions$summary(sbfResults$pred, lev = lev)
      if(is.factor(y)) resamples <- c(resamples, caret:::flatTable(sbfResults$pred$pred, sbfResults$pred$obs))
      resamples <- data.frame(t(resamples))
      resamples$Resample <- names(resampleIndex)[iter]
      
      list(resamples = resamples, selectedVars = sbfResults$variables, pred = tmpPred)
    }

    resamples <- rbind.fill(result[names(result) == "resamples"])
    pred <- if(ctrl$saveDetails) rbind.fill(result[names(result) == "pred"]) else NULL
    performance <- caret:::MeanSD(resamples[,!grepl("Resample", colnames(resamples)),drop = FALSE])
    
    if(ctrl$method %in% c("boot632"))
      {
        modelIndex <- 1:nrow(x)
        holdoutIndex <- modelIndex
        appResults <- caret:::sbfIter(x[modelIndex,,drop = FALSE],
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
    
    list(performance = performance, everything = result, predictions = if(ctrl$saveDetails) pred else NULL)
  }


looSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...)
  {
    library(caret)
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index

    vars <- vector(mode = "list", length = length(y))
    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
    {
      library(caret)

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])
      
      sbfResults <- caret:::sbfIter(x[modelIndex,,drop = FALSE],
                                    y[modelIndex],
                                    x[holdoutIndex,,drop = FALSE],
                                    y[holdoutIndex],
                                    ctrl,
                                    ...)

      sbfResults
    }
    resamples <- do.call("rbind", result[names(result) == "pred"])
    performance <- ctrl$functions$summary(resamples, lev = lev)
    
    list(performance = performance, everything = result, predictions = if(ctrl$saveDetails) resamples else NULL)
  }


################################################################################################

nominalRfeWorkflow <- function(x, y, sizes, ppOpts, ctrl, lev, ...)
  {
    library(caret)
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    if(ctrl$method %in% c("boot632")) resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)

    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
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

      rfeResults <- caret:::rfeIter(x[modelIndex,,drop = FALSE],
                                    y[modelIndex],
                                    x[holdoutIndex,,drop = FALSE],
                                    y[holdoutIndex],
                                    sizes,
                                    ctrl,
                                    label = names(resampleIndex)[iter],
                                    ...)
      resamples <- ddply(rfeResults$pred, .(Variables), ctrl$functions$summary, lev = lev)
      
      if(ctrl$saveDetails)
        {
          rfeResults$pred$Resample <- names(resampleIndex)[iter]
          rfeResults$pred$rowIndex <- rep(seq(along = y)[unique(holdoutIndex)],
                                          length(sizes) - 1)
        }
      
      if(is.factor(y))
        {
          cells <- ddply(rfeResults$pred, .(Variables), function(x) caret:::flatTable(x$pred, x$obs))
          resamples <- merge(resamples, cells)
        }
          
      resamples$Resample <- names(resampleIndex)[iter]
      vars <- do.call("rbind", rfeResults$finalVariables)
      vars$Resample <- names(resampleIndex)[iter]
      list(resamples = resamples, selectedVars = vars, predictions = if(ctrl$saveDetails) rfeResults$pred else NULL)
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
                        caret:::MeanSD,
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
    library(caret)
    ppp <- list(options = ppOpts)
    ppp <- c(ppp, ctrl$preProcOptions)
    
    resampleIndex <- ctrl$index
    result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .packages = "caret", .errorhandling = "stop") %dopar%
    {
      library(caret)

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])
      
      rfeResults <- caret:::rfeIter(x[modelIndex,,drop = FALSE],
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


