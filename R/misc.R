ipredStats <- function(x)
{
  # error check
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
                regression =   c(x$mse[length(x$mse)], x$rsq[length(x$rsq)]),
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


modelLookup <- function(model = NULL)
{
  paramKey <- data.frame(
                         model = c(
                           "treebag",
                           "lm",
                           "lda",
                           "ctree",
                           "ctree2",
                           "cforest",
                           "ada", "ada", "ada",
                           "glmboost", "glmboost",
                           "gamboost", "gamboost",
                           "blackboost", "blackboost",
                           "logitBoost",
                           "nnet", "nnet",
                           "pcaNNet", "pcaNNet",
                           "multinom", 
                           "rda", "rda", 
                           "gbm", "gbm", "gbm",
                           "rfNWS",
                           "rfLSF",
                           "rf",
                           # to keep backwards compat, we have double svm entries
                           "svmpoly", "svmpoly", "svmpoly", 
                           "svmradial", "svmradial",                           
                           "svmPoly", "svmPoly", "svmPoly", 
                           "svmRadial", "svmRadial",
                           "rvmPoly", "rvmPoly", 
                           "rvmRadial",
                           "lssvmPoly", "lssvmPoly", 
                           "lssvmRadial",
                           "gaussprPoly", "gaussprPoly", 
                           "gaussprRadial",                              
                           "gpls", 
                           "lvq", 
                           "rpart", 
                           "pls", 
                           "plsTest",
                           "pam", 
                           "knn", 
                           "nb", 
                           "earth", "earth", 
                           "earthTest", "earthTest",          
                           "mars", "mars",          
                           "bagEarth", "bagEarth",         
                           "fda", "fda",
                           "bagFDA", "bagFDA",
                           "enet", "enet",
                           "lasso",
                           "sddaLDA",
                           "sddaQDA",
                           "J48",
                           "M5Rules",
                           "LMT",
                           "JRip",
                           "lmStepAIC",
                           "slda",
                           "superpc",
                           "superpc"
                           ),
                         parameter = c(
                           "parameter",      
                           "parameter",
                           "parameter",         
                           "mincriterion",
                           "maxdepth",
                           "mtry",
                           "iter", "maxdepth", "nu",
                           "mstop", "prune",
                           "mstop", "prune", 
                           "mstop", "maxdepth",
                           "nIter",
                           "size", "decay",
                           "size", "decay", 
                           "decay", 
                           "gamma", "lambda", 
                           "n.trees", "interaction.depth",  "shrinkage",          
                           "mtry",
                           "mtry",         
                           "mtry", 
                           "C", "degree", "scale",  
                           "C", "sigma",
                           "C", "degree", "scale",  
                           "C", "sigma",
                           "degree", "scale",  
                           "sigma",
                           "degree", "scale",  
                           "sigma",
                           "degree", "scale",  
                           "sigma", 
                           "K.prov", 
                           "k", 
                           "maxdepth", 
                           "ncomp", 
                           "ncomp",          
                           "threshold", 
                           "k", 
                           "usekernel", 
                           "nprune", "degree", 
                           "nprune", "degree",         
                           "nprune", "degree",          
                           "nprune", "degree", 
                           "nprune", "degree",                    
                           "nprune", "degree",
                           "fraction","lambda", 
                           "fraction",
                           "parameter",
                           "parameter",
                           "C",
                           "pruned",
                           "iter",
                           "NumOpt",
                           "parameter",
                           "parameter",
                           "threshold", "n.components"
                           ),
                         label = I(c(
                           "none",      
                           "none",
                           "none",         
                           "P-Value Threshold",
                           "Max Tree Depth",
                           "#Randomly Selected Predictors",
                           "#Trees", "Max Tree Depth", "Learning Rate",
                           "# Boosting Iterations", "AIC Prune?",
                           "# Boosting Iterations", "AIC Prune?",    
                           "#Trees", "Max Tree Depth",
                           "# Boosting Iterations",
                           "#Hidden Units", "Weight Decay",
                           "#Hidden Units", "Weight Decay", 
                           "Weight Decay", 
                           "Gamma", "Lambda", 
                           "#Trees", "Interaction Depth",  "Learning Rate",
                           "#Randomly Selected Predictors",
                           "#Randomly Selected Predictors",         
                           "#Randomly Selected Predictors", 
                           "Cost", "Polynomial Degree", "Scale",  
                           "Cost", "Sigma",
                           "Cost", "Polynomial Degree", "Scale",  
                           "Cost", "Sigma",
                           "Polynomial Degree", "Scale",  
                           "Sigma",
                           "Polynomial Degree", "Scale",  
                           "Sigma",
                           "Polynomial Degree", "Scale",  
                           "Sigma",  
                           "#Components", 
                           "#Prototypes", 
                           "Max Tree Depth", 
                           "#Components", 
                           "#Components",          
                           "Shrinkage Threshold", 
                           "#Neighbors", 
                           "Distribution Type", 
                           "#Retained Terms", "Product Degree",  
                           "#Retained Terms", "Product Degree",                  
                           "#Retained Terms", "Product Degree",  
                           "#Retained Terms", "Product Degree",
                           "#Retained Terms", "Product Degree",                  
                           "#Retained Terms", "Product Degree",
                           "Fraction of Full Solution","Weight Decay",
                           "Fraction of Full Solution",
                           "none",
                           "none",
                           "Confidence Threshold",
                           "Pruned",
                           "# Iteratons",
                           "# Optimizations",
                           "none",
                           "none",
                           "Threshold", "#Components"
                           )),
                         seq = c(
                           FALSE,
                           FALSE,
                           FALSE,
                           TRUE,
                           FALSE,
                           FALSE,
                           FALSE,   FALSE,   FALSE,
                           TRUE,    FALSE,
                           TRUE,    FALSE,   
                           TRUE,    FALSE,
                           TRUE, 
                           FALSE,   FALSE,
                           FALSE,   FALSE, 
                           FALSE, 
                           FALSE,   FALSE, 
                           TRUE,    FALSE,   FALSE,         
                           FALSE,
                           FALSE,         
                           FALSE,
                           FALSE,   FALSE,   FALSE, #svmPoly  
                           FALSE,   FALSE,                              
                           FALSE,   FALSE,   FALSE,  
                           FALSE,   FALSE,          
                           FALSE,   FALSE,   # rvm
                           FALSE,
                           FALSE,   FALSE,   # lssvm
                           FALSE,
                           FALSE,   FALSE,   # gausspr
                           FALSE,
                           FALSE, 
                           FALSE, 
                           TRUE, 
                           TRUE, 
                           FALSE,          
                           TRUE, 
                           FALSE, 
                           FALSE, 
                           TRUE,    FALSE,    # earth        
                           FALSE,   FALSE,  
                           FALSE,   FALSE,
                           FALSE,   FALSE, 
                           FALSE,   FALSE,                 
                           FALSE,   FALSE,
                           TRUE,    FALSE,
                           TRUE,
                           FALSE,              # sdda
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           TRUE,    TRUE       # superpc
                           ),
                         forReg = c(
                           TRUE,
                           TRUE,
                           FALSE,
                           TRUE,
                           TRUE,
                           TRUE,
                           FALSE,   FALSE,   FALSE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           FALSE,
                           TRUE,    TRUE,
                           TRUE,    TRUE, 
                           FALSE, 
                           FALSE,   FALSE, 
                           TRUE,    TRUE,    TRUE,       
                           TRUE,
                           TRUE,         
                           TRUE,         
                           TRUE,    TRUE,    TRUE, 
                           TRUE,    TRUE,
                           TRUE,    TRUE,    TRUE, 
                           TRUE,    TRUE,
                           TRUE,    TRUE, 
                           TRUE,
                           FALSE,   FALSE,    #lssvm
                           FALSE,
                           TRUE,    TRUE, 
                           TRUE,               # guasspr
                           FALSE, 
                           FALSE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           FALSE, 
                           FALSE, 
                           FALSE, 
                           TRUE,    TRUE, 
                           TRUE,    TRUE,           
                           TRUE,    TRUE,          
                           TRUE,    TRUE,         
                           FALSE,   FALSE,
                           FALSE,   FALSE,
                           TRUE,    TRUE,
                           TRUE,
                           FALSE,
                           FALSE,
                           FALSE,
                           TRUE,
                           FALSE,
                           FALSE,
                           TRUE,
                           FALSE,
                           TRUE,    TRUE       # superpc
                           ),               
                         forClass =          
                         c(
                           TRUE,
                           FALSE,
                           TRUE,
                           TRUE,
                           TRUE,#check these
                           TRUE,
                           TRUE,    TRUE,    TRUE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           TRUE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           TRUE, 
                           TRUE,    TRUE, 
                           TRUE,    TRUE,    TRUE,        
                           TRUE,
                           TRUE,           
                           TRUE, 
                           TRUE,    TRUE,    TRUE, #svmPoly 
                           TRUE,    TRUE,
                           TRUE,    TRUE,    TRUE, 
                           TRUE,    TRUE,
                           FALSE,   FALSE,
                           FALSE,
                           TRUE,    TRUE,   # lssvm
                           TRUE,
                           TRUE,    TRUE, 
                           TRUE,
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           TRUE, 
                           FALSE,   FALSE, 
                           FALSE,   FALSE,          
                           FALSE,   FALSE,          
                           FALSE,   FALSE,         
                           TRUE,    TRUE,
                           TRUE,    TRUE,
                           FALSE,   FALSE,
                           FALSE,
                           TRUE,
                           TRUE,
                           TRUE,
                           FALSE,
                           TRUE,
                           TRUE,
                           FALSE,
                           TRUE,
                           FALSE,    FALSE
                           ),
                         probModel = c(
                           TRUE,             #   bagged trees
                           FALSE,            #   lm
                           TRUE,             #   lda
                           TRUE,             #   ctree (1)
                           TRUE,             #   ctree2
                           TRUE,             #   cforest (1)
                           TRUE, TRUE, TRUE, #   ada (3)
                           TRUE, TRUE,       #   glmboost (2)
                           TRUE, TRUE,       #   gamboost (2)
                           TRUE, TRUE,       #   blackboost (2)
                           TRUE,             #   LogitBoost (1),
                           TRUE, TRUE,       #   nnet (2)
                           TRUE, TRUE,       #   pcaNNet (2)
                           TRUE,             #   multinom (1) 
                           TRUE, TRUE,       #   rda (2)
                           TRUE, TRUE, TRUE, #   gbm (3)         
                           TRUE,             #   rfNWS (1)
                           TRUE,             #   rfLSF (1)         
                           TRUE,             #   rf (1) 
                           TRUE, TRUE, TRUE, #   svmPoly (3) 
                           TRUE, TRUE,       #   svmRadial (2)
                           TRUE, TRUE, TRUE, #   svmpoly (3) 
                           TRUE, TRUE,       #   svmradial (2)
                           FALSE,FALSE,      #   rvmPoly (2) 
                           FALSE,            #   rvmRadial (1)
                           FALSE,FALSE,      #   lssvmPoly (2) # not implemented in kernlab
                           FALSE,            #   lssvmRadial (1)
                           TRUE, TRUE,       #   gausspr(2) Poly
                           TRUE,             #   gausspr(1) radial
                           TRUE,             #   gpls (1) 
                           FALSE,            #   lvq (1) 
                           TRUE,             #   rpart (1) 
                           TRUE,             #   pls (1) 
                           TRUE,             #   plsTest (1)          
                           TRUE,             #   pam (1) 
                           TRUE,             #   knn (1) 
                           TRUE,             #   nb (1) 
                           FALSE, FALSE,     #   earth (2)
                           FALSE, FALSE,     #   earthTest (2)         
                           FALSE, FALSE,     #   mars (2)       
                           FALSE, FALSE,     #   bagEarth (2)        
                           TRUE, TRUE,       #   fda (2)
                           TRUE, TRUE,       #   bagFDA (2)
                           FALSE, FALSE,     #   enet (2)
                           FALSE,            #   lasso (1)
                           TRUE,             #   sdda for lda (1)
                           TRUE,             #   sdda for qda (1)
                           TRUE,             #   J48 (1)
                           FALSE,            #   M5Rules(1)
                           TRUE,             #   LMT(1)
                           TRUE,             #   JRip(1)
                           FALSE,            #   stepAIC(0)
                           TRUE,             #   slda(0)
                           FALSE, FALSE      #   superpc(2)

                           ),
                         stringsAsFactors  = FALSE               
                         )         
  
  if(!is.null(model))
    {
      if(!any(model == paramKey$model)) stop("value of model unknown")
      out <- paramKey[paramKey$model == model,]
      
    } else out <- paramKey        
  out     

}


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

  # I've included a pruning technique for models in teh mboost packages. This wouldn't
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
             pls = 
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
             superpc =
             {
               largest <- which(grid$.n.components == max(grid$.n.components) &
                                grid$.threshold == max(grid$.threshold))
               loop <- grid[largest,, drop = FALSE]
               seqParam <- list(grid[-largest,, drop = FALSE])
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

## todo: make avaible to outside
defaultSummary <- function(data, lev = NULL, model = NULL) postResample(data[,"pred"], data[,"obs"])

## This function takes the raw predictions per parameter combination 
## and creates the resampe statistics for each combination

poolByResample <- function(x,                ## The obs and pred outcomes
                           grid,             ## The grid of tuning parameters
                           func,             ## The summary function
                           perfNames = NULL, ## Names of the computed metrics
                           lev,              ## Class levels (NULL if reg)
                           modelName)        ## The model name ("lm", "rf" etc)
{
  k <- nrow(grid)
  for(i in 1:k)
    {
      subGrid <- grid[i,,drop = FALSE]
      subX <- merge(subGrid, x)
      if(nrow(subX) > 0)
        {
          tmp <- by(subX,
                    list(group = subX$group),
                    func,
                    lev = lev,
                    model = modelName)
          
          ## Convert the results from a "by" object to something
          ## that we can use (a matrix). If the summary
          ## function has one output, this will convert it to an
          ## 1xB matrix (B = #resamples). 
          
          resultsPerGroup <- t(sapply(tmp, function(u)u))

          ## We always want an BxM matrix (M = #metrics).
          if(length(perfNames) == 1)
            {
              resultsPerGroup <- t(resultsPerGroup)
              colnames(resultsPerGroup) <- perfNames
            }
          
          resultsPerGroup <- merge(subGrid, resultsPerGroup)
          
          out <- if(!exists("out")) resultsPerGroup else rbind(out, resultsPerGroup)
        }
    }
  out
}

summarize <- function(x, grid)
{
  k <- nrow(grid)
  out <- NULL
  for(i in 1:k)
    {
      subGrid <- grid[i,,drop = FALSE]
      subX <- merge(subGrid, x)
      subX <- subX[,!(names(subX) %in% names(grid)), drop = FALSE]
      perf <- mean(subX, na.rm = TRUE)
      if(nrow(subX) > 1)
        {
          sdPerf <- sd(subX, na.rm = TRUE)
          names(sdPerf) <- paste(names(sdPerf), "SD", sep = "")
          perf <- c(perf, sdPerf)
        }
      tmpPerf <- if(i == 1) perf else rbind(tmpPerf, perf)
    }
  if(is.vector(tmpPerf)) 
    {
      out <- cbind(grid, as.data.frame(t(tmpPerf)))
    } else {
      rownames(tmpPerf) <- paste(1:nrow(tmpPerf))
      out <- cbind(grid, as.data.frame(tmpPerf))
    }
  out
}



byResampleBasic <- function(ind, x, combo = NULL)
{
  isLoo <- all(nrow(x$data) - unlist(lapply(ind, length)) == 1)

  predList <- lapply(ind, modelWrapperBasic, x = x)
  obsList <- lapply(ind, function(index, x) x$data$.outcome[-index], x = x)
  if(!isLoo)
    {
      numPreds <- unlist(lapply(predList, length))   
      rGroup <- paste(
                      "Resample", 
                      rep(seq(along = predList), numPreds),
                      sep = "")


    } else {
      rGroup <- rep("Resample1", length(obsList))
    }
  
  vertical <- data.frame(
                         pred = unlist(predList),
                         obs = unlist(obsList),
                         group = rGroup)
  
  out <- merge(combo, vertical)    
  
  out
}

modelWrapperBasic <- function(ind, x)
{
  trainData <- x$data
  x$data <- x$data[ind,]
  tmpModelFit <- do.call(createModel, x)

  outBagData <- trainData[-ind, ]        
  outBagData$.outcome <- NULL
  
  pred <- predictionFunction(x$method, tmpModelFit, outBagData)
  # todo: To start computing auc ROC, we might want to return a list with
  # probs and preds (where appropriate)
  pred
}

byResampleSeq <- function(ind, x, seq, combo)
{

  isLoo <- all(nrow(x$data) - unlist(lapply(ind, length)) == 1)
  
  # allPred is a list where each element is the results from specific
  # set of help back smaples (e.g. oob samples). In this case, the
  # results are a data frame of values with columns corresponding
  # to different settings for the sequential parameters.
  allPred <- lapply(ind, modelWrapperSeq, x = x, seq)

  # get seq parameter values
  tmpVal <- x$tuneValue[, names(x$tuneValue) %in% names(seq), drop = FALSE]
  seqVals <- rbind(tmpVal, seq) 
  if(x$method == "ctree") seqVals <- seqVals[!duplicated(seqVals),,drop = FALSE]  
  if(!isLoo)
    {
      obsList <- lapply(ind, function(index, x) x$data$.outcome[-index], x = x)
      
      # work across each value of the sequential parameter
      out <- NULL
      for(i in 1:ncol(allPred[[1]]))
        {
          predList <- lapply(allPred, function(u, i) u[,i], i = i)
          numPreds <- unlist(lapply(predList, length))   
          
          vertical <- data.frame(
                                 pred = unlist(predList),
                                 obs = unlist(obsList),
                                 group = paste("Resample", rep(seq(along = predList), times = numPreds), sep = ""))
          
          tmpResults <- merge(seqVals[i,,drop=FALSE], vertical)
          if(!is.null(combo)) tmpResults <- merge(combo, tmpResults)
          out <- if(is.null(out)) tmpResults else rbind(out, tmpResults)
          
        }
    } else {
      # work across each value of the sequential parameter
      out <- NULL
      for(i in 1:ncol(allPred[[1]]))
        {   
          predValues <- unlist(lapply(allPred, function(u, i) u[,i], i = i))
          obsValues <- lapply(ind, function(index, x) x$data$.outcome[-index], x = x)
          
          if(is.factor(x$data$.outcome)) predValues <- factor(as.character(predValues, levels = levels(x$data$.outcome)))
          
          vertical <- data.frame(
                                 pred = predValues,
                                 obs = unlist(obsValues),
                                 group = "Resample1")
          
          tmpResults <- merge(seqVals[i,,drop=FALSE], vertical)
          if(!is.null(combo)) tmpResults <- merge(combo, tmpResults)
          out <- if(is.null(out)) tmpResults else rbind(out, tmpResults)
        }
    }
  out
}


modelWrapperSeq <- function(ind, x, seq)
{
  # also attach param values
  trainData <- x$data
  x$data <- x$data[ind,]
  tmpModelFit <- do.call(createModel, x)

  outBagData <- trainData[-ind, ]        
  oobY <- outBagData$.outcome
  outBagData$.outcome <- NULL
  
  # pass param values here too   
  pred <- predictionFunction(x$method, tmpModelFit, outBagData, seq)

  pred  
}

if (!exists("seq_along")) seq_along <- function(x) seq(along = x)


iterPrint <- function(x, iter)
{
  tmp <- format(x$loop) #[, x$constant, drop = FALSE])
  tmp <- tmp[iter,,drop = FALSE]
  loopVars <- substring(names(tmp), 2)
  tmpString1 <- paste(loopVars, "=", tmp, sep = "")
  tmpString2 <- paste(tmpString1, collapse = ", ")
  cat("Model ", iter, ": ", tmpString2, sep = "")
  if(!is.null(x$vary))
    {
      seqVars <- substring(x$vary, 2)
      if(any(seqVars %in% loopVars))
        {
          cat("\n collapsing over other values of", paste(seqVars, collapse = ", "))
        } else cat("\n collapsing over ", paste(seqVars, collapse = ", "))
    }
  cat("\n")
}

## make this object oriented
getClassLevels <- function(x) 
  {
    if(tolower(x$method) %in% tolower(c("svmRadial", "svmPoly",
                                        "rvmRadial", "rvmPoly",
                                        "lssvmRadial", "lssvmPoly",
                                        "gaussprRadial", "gaussprPoly",
                                        "ctree", "ctree2", "cforest")))
       
      {
        obsLevels <- switch(tolower(x$method),
                            svmradial =, svmpoly =,
                            rvmradial =, rvmpoly =,
                            lssvmradial =, lssvmpoly =, 
                            gaussprpadial =, gaussprpoly =
                            {
                              library(kernlab)
                              lev(x$finalModel)
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

## todo: do we need to add the other kernlab models to the function above?
## there is an error in test cases for predict.train
