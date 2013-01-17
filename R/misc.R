printCall <- function(x)
  {
    call <- paste(deparse(x), collapse = "\n")
    cat("\nCall:\n", call, "\n\n", sep = "")
    ## or

    cat("\nCall:\n", truncateText(deparse(x, width.cutoff = 500)), "\n\n", sep = "")
    invisible(call)
  }

flatTable <- function(pred, obs)
  {
    cells <- as.vector(table(pred, obs))
    names(cells) <- paste(".cell", seq(along= cells), sep = "")
    cells
  }


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
  ## this function extracts information about the requested model and figures 
  ## out the details about how the tuning process should be executed

  if(model != "custom")
    {
      modelInfo <- modelLookup(model)
    } else {
      modelInfo <- data.frame(
                              model = "custom",
                              parameter = gsub("^\\.", "", names(grid)),
                              label = gsub("^\\.", "", names(grid)),
                              seq = FALSE, forReg = TRUE, forClass = TRUE,
                              probModel = TRUE)
      ## TODO pass control in for probModel
    }
  
  ## a little hack hre to change when this goes into production:
  
  if(all(is.na(grid)) & !is.null(grid$.parameter)) grid <- data.frame(.parameter = "none")
  
  ## some models have "sequential" parameters where several different models can be
  ## derived form one R object. For example, in gbm models you can fit a model with
  ## 500 trees and get predictions for any mode with <= 500 trees from the same object

  ## if we don't have any of these types of parameters, use a basic looping strategy
  ## i.e. scheme = "basic"
  if(!any(modelInfo$seq) | useOOB) 
    return(
           list(
                scheme = ifelse(useOOB, "oob", "basic"), 
                loop = grid, 
                seqParam = NULL, 
                model = modelInfo, 
                constant = names(grid), 
                vary = NULL))

  ## I've included a pruning technique for models in the mboost packages. This wouldn't
  ## easily lend itself to a sequential version, so use the basic approach if any 
  ## prune = "yes"
  if(model %in% c("glmboost", "gamboost") && any(grid$.prune == "yes")) modelInfo$seq <- FALSE

  ## some models have sequential parameters, but if the tune grid is manually specified
  ## and there is only 1 value of the sequential parameter(s), we should use the basic
  ## approach
  
  paramVary <- unlist(lapply(grid, function(u) length(unique(u)) > 1))
  paramVary <- data.frame(
                          parameter = substring(names(paramVary), 2),
                          column = names(paramVary),
                          varies = paramVary)
  
  modelInfo <- merge(modelInfo, paramVary)
  modelInfo$varyingSeq <- modelInfo$varies & modelInfo$seq

  scheme <- if(any(modelInfo$varyingSeq)) "seq" else "basic"

  ## if we do have sequential parameters (with more than one value), we need to figure
  ## out what parmeters we should loop over, their values and the values of the 
  ## sequential parameters for each loop

  if(scheme == "seq")
    {
      constant <- as.character(modelInfo$column)[!modelInfo$varyingSeq]
      vary <- as.character(modelInfo$column)[modelInfo$varyingSeq] 
      
      ## The data frame loop is the combination(s) of tuning parameters that we will
      ## be looping over. For each combination in loop, the list seqParam will provide the
      ## value(s) of the sequential parameter that should be evaluated for the same R model
      ## object      
      
      switch(model,
             logitBoost = 
             {
               grid <- grid[order(grid$.nIter, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },             
             pcr =, simpls =, widekernelpls =, pls =, kernelpls =  
             {
               grid <- grid[order(grid$.ncomp, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             leapForward = , leapBackward =, leapSeq =
             {
               grid <- grid[order(grid$.nvmax, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },             
             cubist = 
             {
               grid <- grid[order(-grid$.committees, grid$.neighbors, decreasing = TRUE),, drop = FALSE]
               
               uniqueCom <- unique(grid$.committees)
               
               loop <- data.frame(.committees = uniqueCom)
               loop$.neighbors <- NA
               
               seqParam <- vector(mode = "list", length = length(uniqueCom))
               
               for(i in seq(along = uniqueCom))
                 {
                   subK <- grid[grid$.committees == uniqueCom[i],".neighbors"]
                   loop$.neighbors[loop$.committees == uniqueCom[i]] <- subK[which.max(subK)]
                   seqParam[[i]] <- data.frame(.neighbors = subK[-which.max(subK)])
                 }
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
             C5.0 = 
             {
               loop <- aggregate(
                                 grid$.trials, 
                                 list(
                                      .model = grid$.model, 
                                      .winnow = grid$.winnow), max)
               loop <- decoerce(loop, grid, TRUE)                  
               names(loop)[3] <- ".trials"
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.trials))
                 {
                   index <- which(
                                  grid$.model == loop$.model[i] & 
                                  grid$.winnow == loop$.winnow[i])
                   subTrees <- grid[index, ".trials"] 
                   seqParam[[i]] <- data.frame(.trials = subTrees[subTrees != loop$.trials[i]])
                 }         
             },             
             bstTree = 
             {
               loop <- aggregate(grid$.mstop, 
                                 list(
                                      .maxdepth = grid$.maxdepth, 
                                      .nu = grid$.nu),
                                 max)
               loop <- decoerce(loop, grid, TRUE)                  
               names(loop)[3] <- ".mstop"
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.mstop))
                 {
                   index <- which(
                                  grid$.maxdepth == loop$.maxdepth[i] & 
                                  grid$.nu == loop$.nu[i])
                   subTrees <- grid[index, ".mstop"] 
                   seqParam[[i]] <- data.frame(.mstop = subTrees[subTrees != loop$.mstop[i]])
                 }         
             },
             bstLs =, bstSm =  
             {
               loop <- aggregate(grid$.mstop, 
                                 list(.nu = grid$.nu),
                                 max)
               loop <- decoerce(loop, grid, TRUE)                  
               names(loop)[2] <- ".mstop"
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.mstop))
                 {
                   index <- which(grid$.nu == loop$.nu[i])
                   subTrees <- grid[index, ".mstop"] 
                   seqParam[[i]] <- data.frame(.mstop = subTrees[subTrees != loop$.mstop[i]])
                 }         
             },              
             rpart2 = 
             {
               grid <- grid[order(grid$.maxdepth, decreasing = TRUE),, drop = FALSE]
               loop <- grid[1,,drop = FALSE]
               seqParam <- list(grid[-1,,drop = FALSE])
             },
             rpart = 
             {
               grid <- grid[order(grid$.cp, decreasing = FALSE),, drop = FALSE]
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

             ## About ctree ...
             ## It used to be a seqeuntial model, but teh prediciotn function cannot create
             ## class probabilities in a sequential manner (as it can for predicting the class or
             ## regression output). I left this code here in case that changes, but the seq
             ## flag by modelLookup has been changed to FALSE so this next block never gets
             ## executed.
             ctree = 
             {
               ## there is an exception here:
               ## There does not appear to be a way to tell what value of mincriterion was used
               ## when looking at an object of class BinaryTree. We want to fit a model with the 
               ## smallest mincriterion (the largest tree in the tuning grid), then derive the 
               ## predictions from the smaller trees.
               
               ## Unlike the other models in this function, the seqParam vector *will* include the 
               ## parameter corresponding to the largest tree ( = smallest  mincriterion), but we
               ## will remove this value from the vector in the prediction function
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
             },
             PenalizedLDA =
             {
               loop <- ddply(grid, .(.lambda), function(x) c(.K = max(x$.K)))
               seqParam <- vector(mode = "list", length = nrow(loop))
               for(i in seq(along = loop$.K))
                 {
                   index <- which(grid$.lambda == loop$.lambda[i])
                   subK <- grid[index, ".K"]
                   otherK <- data.frame(.K = subK[subK != loop$.K[i]])
                   if(nrow(otherK) > 0) seqParam[[i]] <- otherK
                 }        
             },
             lda2 =  
             {
               grid <- grid[order(grid$.dimen, decreasing = TRUE),, drop = FALSE]
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




R2 <- function(pred, obs, formula = "corr", na.rm = FALSE)
  {
    n <- sum(complete.cases(pred))
    switch(formula,
           corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
           traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
  }


RMSE <- function(pred, obs, na.rm = FALSE) sqrt(mean((pred - obs)^2, na.rm = na.rm))


defaultSummary <- function(data, lev = NULL, model = NULL)
  {
    if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
    postResample(data[,"pred"], data[,"obs"])
  }

twoClassSummary <- function (data, lev = NULL, model = NULL) 
{
  require(pROC)
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  rocObject <- try(pROC:::roc(data$obs, data[, lev[1]]), silent = TRUE)
  rocAUC <- if(class(rocObject)[1] == "try-error") NA else rocObject$auc
  out <- c(rocAUC,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out) <- c("ROC", "Sens", "Spec")
  out
}

## make this object oriented
getClassLevels <- function(x) 
  {
    if(tolower(x$method) %in% tolower(c("svmRadial", "svmPoly", "svmLinear",
                                        "rvmRadial", "rvmPoly", "rvmLinear",
                                        "lssvmRadial", "lssvmPoly", "lssvmLinear",
                                        "gaussprRadial", "gaussprPoly", "gaussprLinear",
                                        "ctree", "ctree2", "cforest", "svmRadialCost",
                                        "penalized", "Linda", "QdaCov")))
      
      {
        obsLevels <- switch(tolower(x$method),
                            penalized = NULL,
                            svmradial =, svmpoly =, svmlinear =, 
                            rvmradial =, rvmpoly =, svmlinear =,
                            svmradialcost = ,
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

partRuleSummary <- function(x)
  {
    predictors <- all.vars(x$terms)
    predictors <- predictors[predictors != as.character(x$terms[[2]])]
    classes <- levels(x$predictions)
    rules <- capture.output(print(x))
    conditions <- grep("(<=|>=|<|>|=)", rules, value = TRUE)
    classPred <- grep("\\)$", conditions, value = TRUE)
    varUsage <- data.frame(Var = predictors,
                           Overall = 0)
    for(i in seq(along = predictors))
      varUsage$Overall[i] <- sum(grepl(paste("^", predictors[i], sep = ""), conditions))

    numClass <- rep(NA, length(classes))
    names(numClass) <- classes
    for(i in seq(along = classes))
      numClass[i] <- sum(grepl(paste(":", classes[i], sep = " "), classPred))
    
    list(varUsage = varUsage,
         numCond = length(conditions),
         classes = numClass)
    
  }

ripperRuleSummary <- function(x)
  {
    predictors <- all.vars(x$terms)
    predictors <- predictors[predictors != as.character(x$terms[[2]])]
    classes <- levels(x$predictions)
    rules <- capture.output(print(x))
    ## remove header
    rules <- rules[-(1:min(which(rules == "")))]
    conditions <- grep("(<=|>=|<|>|=)", rules, value = TRUE)
    varUsage <- data.frame(Var = predictors,
                           Overall = 0)
    for(i in seq(along = predictors))
      varUsage$Overall[i] <- sum(grepl(paste("\\(", predictors[i], sep = ""), conditions))

    numClass <- rep(NA, length(classes))
    names(numClass) <- classes
    for(i in seq(along = classes))
      numClass[i] <- sum(grepl(paste(x$terms[[2]], "=", classes[i], sep = ""), conditions))
    
    list(varUsage = varUsage,
         numCond = length(conditions),
         classes = numClass)
    
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





smootherFormula <- function(data, smoother = "s", cut = 10, df = 0, span = .5, degree = 1, y = ".outcome")
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
      }
    if(smoother == "lo")
      {
        suffix[numValues > cut] <- paste(", span=", span, ",degree=", degree, ")", sep = "")
      }
    if(smoother == "rcs")
      {
        suffix[numValues > cut] <- ")"
      }    
    rhs <- paste(prefix, names(numValues), suffix, sep = "")
    rhs <- paste(rhs, collapse = "+")
    form <- as.formula(paste(y, rhs, sep = "~"))
    form
  }


varSeq <- function(x)
  {
    vars <- apply(summary(x)$which, 1, function(x) names(which(x)))
    vars <- lapply(vars, function(x) x[x != "(Intercept)"])
    vars
  }



cranRef <- function(x) paste("{\\tt \\href{http://cran.r-project.org/web/packages/", x, "/index.html}{", x, "}}", sep = "")

makeTable <- function(x)
  {
    params <- paste("\\code{", as.character(x$parameter), "}", sep = "", collapse = ", ")
    params <- ifelse(params == "\\code{parameter}", "None", params)

    data.frame(method = as.character(x$model)[1],
               Package = cranRef(as.character(x$Package)[1]),
               Parameters = params)
    

  }

