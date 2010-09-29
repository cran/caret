prettySeq <- function(x) paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")

ipredStats <- function(x)
{
  ## error check
  if(is.null(x$X)) stop("to get OOB stats, keepX must be TRUE when calling the bagging function")
  
  foo <- function(object, y, x)
    {
      holdY <- y[-object$bindx]
      if(is.factor(y))
        {
          library(e1071)
          tmp <- predict(object$btree, x[-object$bindx,], type = "class")
          tmp <- factor(as.character(tmp), levels = levels(y))
          out <- c(
                   mean(holdY == tmp),
                   classAgreement(table(holdY, tmp))$kappa)
          
        } else {
          tmp <- predict(object$btree, x[-object$bindx,])

          out <- c(
                   sqrt(mean((tmp - holdY)^2, na.rm = TRUE)),
                   cor(holdY, tmp, use = "pairwise.complete.obs")^2)
        }
      out    
    }
  eachStat <- lapply(x$mtrees, foo, y = x$y, x = x$X)
  eachStat <- matrix(unlist(eachStat), nrow = length(eachStat[[1]]))
  out <- c(
           apply(eachStat, 1, mean, na.rm = TRUE),
           apply(eachStat, 1, sd, na.rm = TRUE))
  names(out) <- if(is.factor(x$y)) c("Accuracy", "Kappa", "AccuracySD", "KappaSD") else c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
  out
}

rfStats <- function(x)
{
  out <- switch(
                x$type,
                regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                classification = {
                  library(e1071)
                  c(
                    1 - x$err.rate[x$ntree, "OOB"],
                    classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]])
                })
  names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
  out              
}

cforestStats <- function(x)
{
  library(party)
  
  obs <- x@data@get("response")[,1]
  pred <- predict(x,  x@data@get("input"), OOB = TRUE)
  postResample(pred, obs)
  

}

bagEarthStats <- function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5))

tuneScheme <- function(model, grid, useOOB = FALSE)
{
                                        # this function extracts information about the requested model and figures 
                                        # out the details about how the tuning process should be executed

  modelInfo <- modelLookup(model)
  
                                        # a little hack hre to change when this goes into production:
  
  if(all(is.na(grid)) & !is.null(grid$.parameter)) grid <- data.frame(.parameter = "none")
  
                                        # some models have "sequential" parameters where several different models can be
                                        # derived form one R object. For example, in gbm models you can fit a model with
                                        # 500 trees and get predictions for any mode with <= 500 trees from the same object

                                        # if we don't have any of these types of parameters, use a basic looping strategy
                                        # i.e. scheme = "basic"
  if(!any(modelInfo$seq) | useOOB) 
    return(
           list(
                scheme = ifelse(useOOB, "oob", "basic"), 
                loop = grid, 
                seqParam = NULL, 
                model = modelInfo, 
                constant = names(grid), 
                vary = NULL))

                                        # I've included a pruning technique for models in the mboost packages. This wouldn't
                                        # easily lend itself to a sequential version, so use the basic approach if any 
                                        # prune = "yes"
  if(model %in% c("glmboost", "gamboost") && any(grid$.prune == "yes")) modelInfo$seq <- FALSE

                                        # some models have sequential parameters, but if the tune grid is manually specified
                                        # and there is only 1 value of the sequential parameter(s), we should use the basic
                                        # approach
  
  paramVary <- unlist(lapply(grid, function(u) length(unique(u)) > 1))
  paramVary <- data.frame(
                          parameter = substring(names(paramVary), 2),
                          column = names(paramVary),
                          varies = paramVary)
  
  modelInfo <- merge(modelInfo, paramVary)
  modelInfo$varyingSeq <- modelInfo$varies & modelInfo$seq

  scheme <- if(any(modelInfo$varyingSeq)) "seq" else "basic"

                                        # if we do have sequential parameters (with more than one value), we need to figure
                                        # out what parmeters we should loop over, their values and the values of the 
                                        # sequential parameters for each loop

  if(scheme == "seq")
    {
      constant <- as.character(modelInfo$column)[!modelInfo$varyingSeq]
      vary <- as.character(modelInfo$column)[modelInfo$varyingSeq] 
      
                                        # The data frame loop is the combination(s) of tuning parameters that we will
                                        # be looping over. For each combination in loop, the list seqParam will provide the
                                        # value(s) of the sequential parameter that should be evaluated for the same R model
                                        # object      
      
      switch(model,
             logitBoost = 
             {
               grid <- grid[order(grid$.nIter, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },             
             pcr =, pls = 
             {
               grid <- grid[order(grid$.ncomp, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             earth = 
             {
               grid <- grid[order(grid$.degree, grid$.nprune, decreasing = TRUE),, drop = FALSE]
               
               uniqueDegree <- unique(grid$.degree)
               
               loop <- data.frame(.degree = uniqueDegree)
               loop$.nprune <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueDegree))
               
               for(i in seq(along = uniqueDegree))
                 {
                   subNK <- grid[grid$.degree == uniqueDegree[i],".nprune"]
                   loop$.nprune[loop$.degree == uniqueDegree[i]] <- subNK[which.max(subNK)]
                   seqParam[[i]] <- data.frame(.nprune = subNK[-which.max(subNK)])
                 }
             },
             gbm = 
             {
               loop <- aggregate(
                                 grid$.n.trees, 
                                 list(
                                      .interaction.depth = grid$.interaction.depth, 
                                      .shrinkage = grid$.shrinkage), max)
               loop <- decoerce(loop, grid, TRUE)                  
               names(loop)[3] <- ".n.trees"
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.n.trees))
                 {
                   index <- which(
                                  grid$.interaction.depth == loop$.interaction.depth[i] & 
                                  grid$.shrinkage == loop$.shrinkage[i])
                   subTrees <- grid[index, ".n.trees"] 
                   seqParam[[i]] <- data.frame(.n.trees = subTrees[subTrees != loop$.n.trees[i]])
                 }         
             },
             rpart = 
             {
               grid <- grid[order(grid$.maxdepth, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             glmboost =, gamboost =
             {
               grid <- grid[order(grid$.mstop, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1, ".mstop", drop = FALSE])         
             },
             pam = 
             {
               grid <- grid[order(grid$.threshold, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             ctree = 
             {
                                        # there is an exception here:
                                        # There does not appear to be a way to tell what value of mincriterion was used
                                        # when looking at an object of class BinaryTree. We want to fit a model with the 
                                        # smallest mincriterion (the largest tree in the tuning grid), then derive the 
                                        # predictions from the smaller trees.
               
                                        # Unlike the other models in this function, the seqParam vector *will* include the 
                                        # parameter corresponding to the largest tree ( = smallest  mincriterion), but we
                                        # will remove this value from the vector in the prediction function
               loop <- data.frame(.mincriterion = min(grid$.mincriterion))
               seqParam <- list(grid[order(grid$.mincriterion), ".mincriterion",drop = FALSE])
             },
             blackboost = 
             {
               loop <- aggregate(grid$.mstop, list(.maxdepth = grid$.maxdepth), max)
               names(loop)[2] <- ".mstop"
               loop <- decoerce(loop, grid, TRUE)                              
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.mstop))
                 {
                   index <- which(grid$.maxdepth == loop$.maxdepth[i])
                   subStops <- grid[index, ".mstop"] 
                   seqParam[[i]] <- data.frame(.mstop = subStops[subStops != loop$.mstop[i]])
                 }        
             },
             enet = 
             {
               grid <- grid[order(grid$.lambda, grid$.fraction, decreasing = TRUE),, drop = FALSE]
               
               uniqueLambda <- unique(grid$.lambda)
               
               loop <- data.frame(.lambda = uniqueLambda)
               loop$.fraction <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueLambda))
               
               for(i in seq(along = uniqueLambda))
                 {
                   subFrac <- grid[grid$.lambda == uniqueLambda[i],".fraction"]
                   loop$.fraction[loop$.lambda == uniqueLambda[i]] <- subFrac[which.max(subFrac)]
                   seqParam[[i]] <- data.frame(.fraction = subFrac[-which.max(subFrac)])
                 }         
             },             
             lasso = 
             {
               grid <- grid[order(grid$.fraction, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             penalized = 
             {
               grid <- grid[order(grid$.lambda1, grid$.lambda2, decreasing = TRUE),, drop = FALSE]
               
               uniqueLambda2 <- unique(grid$.lambda2)
               
               loop <- data.frame(.lambda2 = uniqueLambda2)
               loop$.lambda1 <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueLambda2))
               
               for(i in seq(along = uniqueLambda2))
                 {
                   subL1 <- grid[grid$.lambda2 == uniqueLambda2[i],".lambda2"]
                   loop$.lambda1[loop$.lambda2 == uniqueLambda2[i]] <- subL1[which.max(subL1)]
                   seqParam[[i]] <- data.frame(.lambda1 = subL1[-which.max(subL1)])
                 }         
             },             
             superpc =
             {
               largest <- which(grid$.n.components == max(grid$.n.components) &
                                grid$.threshold == max(grid$.threshold))
               loop <- grid[largest,, drop = FALSE]
               seqParam <- list(grid[-largest,, drop = FALSE])
             },
             glmnet =
             {  
               uniqueAlpha <- unique(grid$.alpha)
               
               loop <- data.frame(.alpha = uniqueAlpha)
               loop$.lambda <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueAlpha))
               
               for(i in seq(along = uniqueAlpha))
                 {
                   seqParam[[i]] <- data.frame(.lambda = subset(grid, subset = .alpha == uniqueAlpha[i])$.lambda)
                 } 
             },
             relaxo =
             {

               loop <- aggregate(
                                 grid$.lambda, 
                                 list(.phi = grid$.phi),
                                 max)
               names(loop) <- c(".phi", ".lambda")
               
               seqParam <- vector(mode = "list", length = nrow(loop))
               
               for(i in seq(along = seqParam))
                 {
                   seqParam[[i]] <- data.frame(.lambda = subset(grid,
                                                 subset = .phi == loop$.phi[i] & .lambda < loop$.lambda[i])$.lambda)
                 } 
             },
             lars = 
             {
               grid <- grid[order(grid$.fraction, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             lars2 = 
             {
               grid <- grid[order(grid$.step, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             foba = 
             {
               grid <- grid[order(grid$.lambda, grid$.k, decreasing = TRUE),, drop = FALSE]
               
               uniqueLambda <- unique(grid$.lambda)
               
               loop <- data.frame(.lambda = uniqueLambda)
               loop$.k <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueLambda))
               
               for(i in seq(along = uniqueLambda))
                 {
                   subK <- grid[grid$.lambda == uniqueLambda[i],".k"]
                   loop$.k[loop$.lambda == uniqueLambda[i]] <- subK[which.max(subK)]
                   seqParam[[i]] <- data.frame(.k = subK[-which.max(subK)])
                 }         
             },
             partDSA = 
             {
               grid <- grid[order(grid$.MPD, grid$.cut.off.growth, decreasing = TRUE),, drop = FALSE]
               
               uniqueMPD <- unique(grid$.MPD)
               
               loop <- data.frame(.MPD = uniqueMPD)
               loop$.cut.off.growth <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueMPD))
               
               for(i in seq(along = uniqueMPD))
                 {
                   subCuts <- grid[grid$.MPD == uniqueMPD[i],".cut.off.growth"]
                   loop$.cut.off.growth[loop$.MPD == uniqueMPD[i]] <- subCuts[which.max(subCuts)]
                   seqParam[[i]] <- data.frame(.cut.off.growth = subCuts[-which.max(subCuts)])
                 }
             },
             scrda = 
             {
               grid <- grid[order(grid$.alpha, grid$.delta, decreasing = TRUE),]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             }            
             )
      out <- list(scheme = "seq", loop = loop, seqParam = seqParam, model = modelInfo, constant = constant, vary = vary)
    } else out <- list(scheme = "basic", loop = grid, seqParam = NULL, model = modelInfo, constant = names(grid), vary = NULL)
  
  out
}


decoerce <- function(x, grid, dot = FALSE)
{
  for(k in names(grid))
    {
      origMode <- mode(grid[, k])
      paramName <- if(dot) k else substring(k, 2)        
      if(any(names(x) %in% paramName) & !is.factor(grid[, k]))
        {
          x[, paramName] <- switch(origMode,
                                   numeric = as.numeric(as.character(x[, paramName])),
                                   character = as.character(x[, paramName]),
                                   logical = as.logical(x[, paramName]))
        }
    }
  x
}  


defaultSummary <- function(data, lev = NULL, model = NULL)
  {
    if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
    postResample(data[,"pred"], data[,"obs"])
  }

twoClassSummary <- function(data, lev = NULL, model = NULL)
  {
    out <- c(sensitivity(data[, "pred"], data[, "obs"], lev[1]),
             specificity(data[, "pred"], data[, "obs"], lev[2]),
             aucRoc(roc(data[, lev[1]], data$obs, positive = lev[1])))
    
    names(out) <- c("Sens", "Spec", "ROC")
    out
  }

## make this object oriented
getClassLevels <- function(x) 
  {
    if(tolower(x$method) %in% tolower(c("svmRadial", "svmPoly", "svmLinear",
                                        "rvmRadial", "rvmPoly", "rvmLinear",
                                        "lssvmRadial", "lssvmPoly", "lssvmLinear",
                                        "gaussprRadial", "gaussprPoly", "gaussprLinear",
                                        "ctree", "ctree2", "cforest",
                                        "penalized", "Linda", "QdaCov")))
      
      {
        obsLevels <- switch(tolower(x$method),
                            penalized = NULL,
                            svmradial =, svmpoly =, svmlinear =, 
                            rvmradial =, rvmpoly =, svmlinear =,
                            lssvmradial =, lssvmpoly =,  lssvmlinear =,
                            gaussprpadial =, gaussprpoly =, gaussprlinear =
                            {
                              library(kernlab)
                              lev(x$finalModel)
                            },

                            linda =, qdacov = 
                            {
                              names(x$finalModel@prior)
                            },
                            
                            ctree =, ctree2 =, cforest =
                            {
                              library(party)
                              levels(x$finalModel@data@get("response")[,1])
                            })
      } else {
        obsLevels <- x$finalModel$obsLevels
      }
    obsLevels
  }


##########################################################################################################

## splitIndicies takes a number of tasks (n) and divides it into k groups
## of roughly equal size. The result is an integer vector of task groups

splitIndicies <- function(n, k)
  {
    out <- rep(1:k, n%/%k)
    if(n %% k > 0)  out <- c(out, sample(1:k, n %% k))
    sort(out)
  }

## workerData takes a series of model specifications (in the loop object) and a set
## of resampling index vectors (in the list called index) and creates a single list that
## will be used to build models. This will allow the computations to be setup across several
## different workers. For example, if we wish to fit 5 different RDA classification models
## across 50 bootstrap samples using 10 workers, workerData produces a list of 10x5 = 50
## elements (each element does the work for 10 bootstrap samples).

## The workers can use lapply to do the computations across different computers or processors
## as specified.

## Note that the number of workers is need so that we can minimize the number of copies of the
## raw data. We only need one per worker.

workerData <- function(data, ctrl, loop, method, lvls, pp, workers = 1, caretVerbose, ...)
  {

    index <- ctrl$index
    
    if(loop$scheme != "oob")
      {
        ## Here we want to split the number of total tasks (B times M where B=#resamples and
        ## M=#models) into W times M groups (W=#workers).

        ## Each worker's "task" will potentially be one model specification* done across one
        ## or more resampled data set. In this way, only one copy of the data is made per worker
        ##
        ## * with the excpetion being "sequential" models with one specification of "fixed" model
        ##   parameters that yeild multiple models
        
        subsets <- vector(mode = "list",
                          length = nrow(loop$loop) * workers)

        subsets <- lapply(subsets,
                          function(x, m, l, d)
                          list(data = NULL,                 ##  constant but will be added later
                               index = NULL,
                               method = m,                  ## constant across the workers
                               fixed = NULL,
                               obsLevels = l,               ## constant across the workers
                               seq = NULL,
                               worker = NULL,
                               isLOO = ifelse(ctrl$method == "LOOCV",
                                 TRUE, FALSE),              ## constant across the workers
                               classProbs = ctrl$classProbs,## constant across the workers
                               func = ctrl$summaryFunction, ## constant across the workers
                               preProc = list(options = pp, ## constant across the workers
                                              thresh = ctrl$PCAthresh,
                                              ica = ctrl$ICAcomp),                
                               caretVerbose = caretVerbose, ## constant across the workers
                               dots = d),                   ## constant across the workers
                          m = method,
                          l = lvls,
                          d = list(...))

        workLoads <- splitIndicies(length(index), workers)
        
        iter <- 0
        for(i in 1:nrow(loop$loop))
          {
            for(j in 1:workers)
              {
                iter <- iter + 1
                subsets[[iter]]$index <- index[workLoads == j]
                subsets[[iter]]$fixed <- loop$loop[i,,drop = FALSE]
                subsets[[iter]]$worker <- paste("W", j, sep = "")
                if(!is.null(loop$seq)) subsets[[iter]]$seq <- loop$seq[[i]]
              }
          }

        ## I don't see any way around this in general
        ## I did this last to try to save time on the previous steps
        subsets <- lapply(subsets,
                          function(x, d) {x$data <- d; x},
                          d = data)

      } else {
        ## Here we will make all make a list element for all M combos since
        ## the same data set will be used for each. This is memory inefficent
        ## compared to the non-OOB strategy (since we create more copies of
        ## the data than needed), but it let's us fit the code into a common
        ## computational scheme.

        ## Currently, this assumes that there are no sequential models where
        ## we would compute OOB error rates.
        
        subsets <- vector(mode = "list",
                          length = nrow(loop$loop))

        subsets <- lapply(subsets,
                          function(x, m, l, d)
                          list(data = NULL,
                               method = m,
                               fixed = NULL,
                               obsLevels = l,
                               caretVerbose = caretVerbose,
                               dots = d),
                          m = method,
                          l = lvls,
                          d = list(...))
        iter <- 0
        for(i in 1:nrow(loop$loop))
          {
            for(j in 1:workers)
              {
                iter <- iter + 1
                subsets[[iter]]$fixed <- loop$loop[i,,drop = FALSE]
              }
          }

        subsets <- lapply(subsets,
                          function(x, d) {x$data <- d; x},
                          d = data)
      }
    subsets
  }


## workerTasks is a function to fit multiple models across multiple datasets. It needs
## a list of objects produced by the workerData function and returns a data frame of
## predictions for the held-out data.

## It returns the predictions instead of the summaries because summaries cannot be done
## by the workers in a few cases (such as leave-one-out cv). Also, it returns a data frame
## instead of a matrix because the columns may not be of the same mode. For example, we also
## return an indicator for which resample data set the prediction is for and also the model
## specification for each prediction. In general, these columns would not be the same format.
## todo: In the future, I plan to (optionally) add the ability to return class probabilites if the
## use asks for them.

workerTasks <- function(x)
  {
  
    ## If this function is being executed remotly, check to see if the package is loaded
    if(!("caret" %in% loadedNamespaces())) library(caret)

    ## This function recreates the model specifications
    expand <- function(fixed, seq)
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

    if(x$caretVerbose)
      {
        modelInfo <- paste(gsub("^\\.", "", names(x$fixed)),
                           format(x$fixed),
                           sep = "=")
        cat("Fitting:",
            paste(modelInfo, collapse = ", "),
            "\n")
      } 

    params <- expand(x$fixed, x$seq)

    ## For ctree, we had to repeat the first value
    if(x$method == "ctree") params <- params[!duplicated(params),, drop = FALSE]

    ## For glmnet, we fit all the lambdas but x$fixed has an NA
    if(x$method == "glmnet") params <- params[complete.cases(params),, drop = FALSE]
    
    numResamples <- length(x$index)

    if(numResamples == 0 & x$method %in% c("rf", "treebag", "cforest", "bagEarth", "bagFDA"))
      {
        args <- list(data = x$data,
                     method = x$method,
                     tuneValue = x$fixed,
                     pp = x$preProc,
                     obsLevels = x$obsLevels)
        if(length(x$dots) > 0) args <- c(args, x$dots)

        modelObj <- do.call(createModel, args)

        ## NOTE: in the case of oob resampling, we return the summarized performance
        ## instead of the predicted values
        out <- switch(
                      class(modelObj$fit)[1],
                      randomForest = rfStats(modelObj$fit),
                      RandomForest = cforestStats(modelObj$fit),
                      bagEarth =, bagFDA = bagEarthStats(modelObj$fit),
                      regbagg =, classbagg = ipredStats(modelObj$fit))
        out <- cbind(as.data.frame(t(out)), params)
        
      } else {
        
        for(i in 1:numResamples)
          {            
            args <- list(data = x$data[x$index[[i]],],
                         method = x$method,
                         tuneValue = x$fixed,
                         pp = x$preProc,
                         obsLevels = x$obsLevels)
            if(length(x$dots) > 0) args <- c(args, x$dots)

            modelObj <- do.call(createModel, args)

            ## Check to see if the index is the entire data set
            ## and number of resamples is 1.
            ## If yes, then make holdback the entire data set to
            ## get apparent error rate
            if(length(x$index) == 1 & length(x$index[[1]]) == nrow(x$data))
              {
                holdBack <-  if(all.equal(sort(x$index[[1]]), seq(along  = x$data$.outcome))) x$data else x$data[-x$index[[i]],]
              } else holdBack <-  x$data[-x$index[[i]],]
            observed <- holdBack$.outcome
            holdBack$.outcome <- NULL
            if(any(colnames(holdBack) == ".modelWeights")) holdBack$.modelWeights <- NULL

            ## If we have seqeuntial parameters, predicted is a list, otherwise
            ## it is a vector            
            predicted <- caret:::predictionFunction(method = x$method,
                                                    modelFit = modelObj$fit,
                                                    newdata = holdBack,
                                                    preProc = modelObj$preProc,
                                                    param = x$seq)
            if(x$classProbs)
              {
                probValues <- caret:::probFunction(method = x$method,
                                                    modelFit = modelObj$fit,
                                                    newdata = holdBack,
                                                    preProc = modelObj$preProc,
                                                    param = x$seq)
              }

            if(!x$isLOO)
              {

                if(!is.null(x$seq))
                  {
                    predicted <- lapply(predicted,
                                        function(x, y, lv)
                                        {
                                          if(is.factor(x)) y <- factor(as.character(y),
                                                                       levels = lv)
                                          data.frame(pred = x, obs = y)
                                        },
                                        y = observed,
                                        lv = x$obsLevels)
                    if(x$classProbs)
                      {
                        for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                      }

                    thisResample <-  lapply(predicted,
                                            x$func,
                                            lev = x$obsLevels,
                                            model = x$method)

                    pNames <- names(thisResample[[1]])
                    ##On 9/10/10 10:24 AM, "Hadley Wickham" <hadley@rice.edu> wrote:
                    thisResample <-matrix(unlist(thisResample),
                                          nrow = length(thisResample),
                                          dimnames = list(NULL, NULL),
                                          byrow = TRUE)
                    colnames(thisResample) <- pNames
                    thisResample <- cbind(as.data.frame(thisResample), params)
                    thisResample$Resample <- names(x$index)[i]                  
                  } else {
                    if(is.factor(observed)) predicted <- factor(as.character(predicted),
                                                                levels = x$obsLevels)
                    tmp <-  data.frame(pred = predicted,
                                       obs = observed)
                    if(x$classProbs) tmp <- cbind(tmp, probValues)
                                        #browser()
                    thisResample <- x$func(tmp,
                                           lev = x$obsLevels,
                                           model = x$method)
                    thisResample <- as.data.frame(t(thisResample))
                    thisResample <- cbind(thisResample, params)
                    thisResample$Resample <- names(x$index)[i]
                  }
                out <- if(i == 1) thisResample else rbind(out, thisResample)
              } else {
                if(!is.null(x$seq))
                  {
                    predicted <- lapply(predicted,
                                        function(x, y, lv)
                                        {
                                          if(is.factor(x)) y <- factor(as.character(y),
                                                                       levels = lv)
                                          data.frame(pred = x, obs = y)
                                        },
                                        y = observed,
                                        lv = x$obsLevels)
                    
                    thisResample <- do.call("rbind", predicted)
                    thisResample <- cbind(thisResample, params)
                    thisResample$Resample <- names(x$index)[i]   

                  } else {
                    if(!is.factor(observed))
                      {
                        thisResample <- data.frame(obs  = observed,
                                                   pred = predicted)
                      } else {
                        thisResample <- data.frame(obs  = factor(as.character(observed),  levels = x$obsLevels),
                                                   pred = factor(as.character(predicted), levels = x$obsLevels))
                      }
                    thisResample$Resample <- names(x$index)[i]
                    thisResample <- cbind(thisResample, params)
                    
                  }
                out <- if(i == 1) thisResample else rbind(out, thisResample)
              }
          }
      }
    out
  }


looSummary <- function(x, func, param)
  {
    out <- data.frame(t(func(x)))
    paramValues <- x[, param, drop = FALSE]
    paramValues <- paramValues[!duplicated(paramValues),,drop = FALSE]
    colnames(paramValues) <- param
    cbind(out, paramValues)
  }

performanceSummary <- function(x, param, method)
  {
    if(method == "LOOCV") return(x)
    byGroups <- vector(mode = "list", length = length (param))
    for(i in seq(along = param)) byGroups[[i]] <- x[,param[i]]
    names(byGroups) <- param
    perfCols <- names(x)[!(names(x) %in% c("Resample", param))]
    mns <- aggregate(x[, perfCols, drop = FALSE],
              byGroups,
              function(x) mean(x, na.rm = TRUE))
    sds <- aggregate(x[, perfCols, drop = FALSE],
              byGroups,
              function(x) sd(x, na.rm = TRUE))
    names(sds)[names(sds) %in% perfCols] <- paste(perfCols, "SD", sep = "")
    merge(mns, sds)
  }


getPerformance <- function(x, groups, func, levels, mod, loo = FALSE)
  {

    if(!loo) groups <- c(groups, "Resample")

    if(length(levels) > 1 && !any(is.na(levels)))
      {
        x$obs  <- factor(as.character(x$obs), levels = levels)
        x$pred <- factor(as.character(x$pred), levels = levels)
      }

    x$parameterGroup <- factor(
                               apply(x[, groups, drop = FALSE],
                                     1,
                                     function(x) paste(x, collapse = "--")))

    splitUp <- split(x, x$parameterGroup)

    ## Write a wrapper around func to also get resample number
    func2 <- function(x, l, m, g)
      {
        out <- func(x, l, m)
        out <- as.matrix(t(out))
        out <- as.data.frame(out)

        for(i in seq(along = g))
          {
            out$..tmp <- unique(x[,g[i]])[1]
            names(out)[ncol(out)] <- g[i]
          }
        
        out
      }

    result1 <- lapply(splitUp, func2, l = levels, m = mod, g = groups)
    result1 <- do.call("rbind", result1)
    perfNames <- names(result1)[!(names(result1) %in% groups)]

    if(!loo)
      {
        groups <- groups[groups != "Resample"]
        result1$parameterGroup <- factor(
                                         apply(result1[, groups, drop = FALSE],
                                               1,
                                               function(x) paste(x, collapse = "--")))

        splitUp <- split(result1, result1$parameterGroup)

        meanFunc <- function(x, y, p)
          {
            x <- x[, p, drop = FALSE]
            mean(x, na.rm = TRUE)
          }
        means <- lapply(splitUp, meanFunc, y = groups, p = perfNames)
        means <- do.call("rbind", means)

        sdFunc <- function(x, y, p)
          {
            x <- x[, p, drop = FALSE]
            sd(x, na.rm = TRUE)
          }
        sds <- lapply(splitUp, sdFunc, y = groups, p = perfNames)
        sds <- do.call("rbind", sds)
        colnames(sds) <- paste( colnames(sds), "SD", sep = "")

        groupValues <- lapply(splitUp, function(x, y) x[1,y,drop = FALSE], y = groups)
        groupValues <- do.call("rbind", groupValues)
        colnames(groupValues) <- gsub("^\\.", "", colnames(groupValues))
        
        out <- cbind(groupValues, means, sds)
        rownames(out) <- 1:nrow(out)
        result1$parameterGroup <- NULL
        rownames(result1) <- 1:nrow(result1)
        return(list(values = result1, results = out))
      } else {
        rownames(result1) <- 1:nrow(result1)
        colnames(result1) <- gsub("^\\.", "", colnames(result1))
        return(list(values = NULL, results = result1))
      }
    stop("error in pooling performance")

  }

## This makes a list of copies of another list


repList <- function(x, times = 3, addIndex = FALSE)
  { 
    out <- vector(mode = "list", length = times)
    out <- lapply(out, function(a, b) b, b = x)
    if(addIndex) for(i in seq(along = out)) out[[i]]$.index <- i
    out
  }

useMathSymbols <- function(x)
  {
    if(x == "Rsquared") x <- expression(R^2)
    x
  }

depth2cp <- function(x, depth)
  {
    out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
    out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
    out
  }





gamFormula <- function(data, smoother = "s", cut = 10, df = 0, span = .5, degree = 1)
  {
    nzv <- nearZeroVar(data)
    if(length(nzv) > 0) data <- data[, -nzv, drop = FALSE]

    numValues <- sort(apply(data, 2, function(x) length(unique(x))))
    prefix <- rep("", ncol(data))
    suffix <- rep("", ncol(data))
    prefix[numValues > cut] <- paste(smoother, "(", sep = "")
    if(smoother == "s")
      {
        
        suffix[numValues > cut] <- if(df == 0) ")" else paste(", df=", df, ")", sep = "")
      } else {
        suffix[numValues > cut] <- paste(", span=", span, ",degree=", degree, ")", sep = "")
      }
    rhs <- paste(prefix, names(numValues), suffix, sep = "")
    rhs <- paste(rhs, collapse = "+")
    form <- as.formula(paste(".outcome~", rhs, sep = ""))
    form
  }




