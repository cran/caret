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
                           "parRF",
                           ## to keep backwards compat, we have double svm entries
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
                           "superpc",
                           "ppr",
                           "sparseLDA", "sparseLDA",
                           "penalized", "penalized",
                           "spls", "spls", "spls",
                           "sda",
                           "glm",
                           "mda",
                           "pda",
                           "pda2",
                           "qda",
                           "glmnet", "glmnet",
                           "relaxo", "relaxo",
                           "lars",
                           "lars2",
                           "OneR",
                           "PART",
                           "PART",
                           "rlm",
                           "svmLinear",
                           "rvmLinear",
                           "lssvmLinear",
                           "gaussprLinear",
                           "vbmpRadial",
                           "smda", "smda", "smda",
                           "pcr",
                           "obliqueTree", "obliqueTree",
                           "nodeHarvest", "nodeHarvest",
                           "Linda",
                           "QdaCov",
                           "glmrob",
                           "stepLDA", "stepLDA",
                           "stepQDA", "stepQDA",
                           "plr", "plr",
                           "GAMens", "GAMens", "GAMens"
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
                           "threshold", "n.components",
                           "nterms",
                           "NumVars", "lambda",
                           "lambda1", "lambda2",
                           "K", "eta", "kappa",
                           "diagonal",
                           "parameter",
                           "subclasses",
                           "lambda",
                           "df",
                           "parameter",
                           "lambda", "alpha",
                           "lambda", "phi",
                           "fraction",
                           "step",
                           "parameter",
                           "threshold",
                           "pruned",
                           "parameter",
                           "C",
                           "parameter",
                           "parameter",
                           "parameter",
                           "estimateTheta",
                           "NumVars", "R", "lambda",
                           "ncomp",
                           "oblique.splits", "variable.selection",
                           "maxinter", "mode",      
                           "parameter",
                           "parameter",
                           "parameter",
                           "maxvar", "direction",
                           "maxvar", "direction",
                           "lambda", "cp",
                           "iter", "rsm_size", "fusion"
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
                           "Threshold", "#Components",
                           "# Terms",
                           "# Predictors", "Lambda",
                           "L1 Penalty", "L2 Penalty",
                           "#Components", "Threshold", "Kappa",
                           "Diagonalize",
                           "none",
                           "#Subclasses Per Class",
                           "Shrinkage Penalty Coefficient",
                           "Degrees of Freedom",
                           "none",
                           "Regularization Parameter",
                           "Mixing Percentage",
                           "Penalty Parameter",
                           "Relaxation Parameter",
                           "Fraction",
                           "#Steps",
                           "none",
                           "Confidence Threshold",
                           "Pruning",
                           "none",
                           "C",
                           "none",
                           "none",
                           "none",
                           "Theta Estimated",
                          "# Predictors",  "# Subclasses", "Lambda",
                           "#Components",
                           "Oblique Splits", "Variable Selection Method",
                           "Maximum Interaction Depth", "Prediction Mode",      
                           "none",
                           "none",
                           "none",
                           "Maximum #Variables", "Search Direction" ,
                           "Maximum #Variables", "Search Direction",
                           "L2 Penalty", "Complexity Parameter",
                           "Ensemble Size", "#Random Feature Subsets", "Data Fusion Function"
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
                           FALSE,#
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
                           TRUE,    TRUE,       # superpc
                           FALSE,
                           FALSE,   FALSE,
                           FALSE,    FALSE,     ## penalized; see note in createModel,
                           FALSE,    FALSE,    FALSE,
                           FALSE,
                           FALSE,              ## glm
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           TRUE,  FALSE,
                           TRUE,  FALSE,
                           TRUE,
                           TRUE,
                           FALSE,
                           FALSE, FALSE,
                           FALSE,               ## rlm
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE, FALSE, FALSE,
                           TRUE,
                           FALSE, FALSE,
                           FALSE, FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE, FALSE,
                           FALSE, FALSE,
                           FALSE, FALSE,
                           FALSE, FALSE, FALSE
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
                           TRUE, 
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
                           TRUE,    TRUE,       # superpc
                           TRUE,
                           FALSE,   FALSE,
                           TRUE,    TRUE,
                           TRUE,    TRUE,   TRUE,
                           FALSE,
                           TRUE,                 ## glm
                           FALSE,
                           FALSE,
                           FALSE,
                           FALSE,
                           TRUE,  TRUE,
                           TRUE, TRUE,
                           TRUE,
                           TRUE,
                           FALSE,
                           FALSE,  FALSE,
                           TRUE,               ## rlm
                           TRUE,               ## svm linear
                           TRUE,               ## rvn linear
                           FALSE,              ## ls svm linear
                           TRUE,               ## gaussian linear
                           FALSE,
                           FALSE, FALSE, FALSE,
                           TRUE,               ## PCR
                           FALSE, FALSE,
                           TRUE,  TRUE,
                           FALSE,
                           FALSE,
                           TRUE,
                           FALSE, FALSE,
                           FALSE, FALSE,
                           FALSE, FALSE,
                           FALSE, FALSE, FALSE
                           ),               
                         forClass =          
                         c(
                           TRUE,
                           FALSE,
                           TRUE,
                           TRUE,
                           TRUE, ##check these
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
                           TRUE,
                           TRUE,    TRUE,    TRUE, ##svmPoly 
                           TRUE,    TRUE,
                           TRUE,    TRUE,    TRUE, 
                           TRUE,    TRUE,
                           FALSE,   FALSE,
                           FALSE,
                           TRUE,    TRUE,   ## lssvm
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
                           TRUE,   TRUE, 
                           FALSE,   FALSE,          
                           FALSE,   FALSE,          
                           TRUE,   TRUE,         
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
                           FALSE,    FALSE,
                           FALSE,
                           TRUE,     TRUE,
                           FALSE,    FALSE, ## penalized has no way to pass the class levels in
                           TRUE,     TRUE,    TRUE,
                           TRUE,
                           TRUE,      ## glm
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,  TRUE,
                           FALSE, FALSE,
                           FALSE,
                           FALSE,
                           TRUE,
                           TRUE,  TRUE,
                           FALSE,              ## rlm
                           TRUE,               ## svm linear
                           FALSE,              ## rvm linear
                           TRUE,               ## ls svm linear
                           TRUE,               ## gaussian linear
                           TRUE,
                           TRUE, TRUE, TRUE,
                           FALSE,
                           TRUE, TRUE,
                           TRUE, TRUE,
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE, TRUE,
                           TRUE, TRUE,
                           TRUE, TRUE,
                           TRUE, TRUE, TRUE
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
                           TRUE,             #   parRF(1)
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
                           TRUE,  TRUE,      #   earth (2)
                           FALSE, FALSE,     #   earthTest (2)         
                           FALSE, FALSE,     #   mars (2)       
                           TRUE,  TRUE,      #   bagEarth (2)        
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
                           FALSE, FALSE,     #   superpc(2)
                           FALSE,
                           TRUE,  TRUE,      #   sparseLDA,
                           FALSE, FALSE,     #   penalized
                           TRUE,  TRUE, TRUE,#   splsda (3)
                           TRUE,             #   sda
                           TRUE,             #   glm
                           TRUE,             #   mda
                           TRUE,
                           TRUE,
                           TRUE,
                           TRUE,  TRUE,       # glmnet
                           FALSE, FALSE,      # relaxo,
                           FALSE,             # lars
                           FALSE,             # lars2
                           TRUE,              # OneR
                           TRUE, TRUE,        # PART
                           FALSE,              ## rlm
                           TRUE,               ## svm linear
                           FALSE,              ## rvm linear
                           FALSE,              ## ls svm linear
                           TRUE,               ## gaussian linear
                           TRUE,
                           FALSE, FALSE, FALSE, ## not yet
                           FALSE,              ## pcr
                           TRUE, TRUE,
                           TRUE, TRUE,
                           TRUE,              ## Linda
                           TRUE,              ## QdaCov
                           TRUE,              ## glmrob
                           TRUE, TRUE,        ## stepLDA
                           TRUE, TRUE,        ## plr
                           TRUE, TRUE,        ## stepQDA
                           TRUE, TRUE, TRUE   ## GAMens
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
defaultSummary <- function(data, lev = NULL, model = NULL)
  {
    if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
    postResample(data[,"pred"], data[,"obs"])
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

workerData <- function(data, index, loop, method, lvls, workers = 1, caretVerbose, ...)
  {

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
                          list(data = NULL,                ##  constant but will be added later
                               index = NULL,
                               method = m,                  ## constant across the workers
                               fixed = NULL,
                               obsLevels = l,               ## constant across the workers
                               seq = NULL,
                               worker = NULL,
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

    numResamples <- length(x$index)

    if(numResamples == 0 & x$method %in% c("rf", "treebag", "cforest", "bagEarth", "bagFDA"))
      {
        args <- list(data = x$data,
                     method = x$method,
                     tuneValue = x$fixed,
                     obsLevels = x$obsLevels)
        if(length(x$dots) > 0) args <- c(args, x$dots)

        modelObj <- do.call(createModel, args)

        ## NOTE: in the case of oob resampling, we return the summarized performance
        ## instead of the predicted values
        out <- switch(
                      class(modelObj)[1],
                      randomForest = rfStats(modelObj),
                      RandomForest = cforestStats(modelObj),
                      bagEarth =, bagFDA = bagEarthStats(modelObj),
                      regbagg =, classbagg = ipredStats(modelObj))
        
      } else {
        
        for(i in 1:numResamples)
          {

            
            args <- list(data = x$data[x$index[[i]],],
                         method = x$method,
                         tuneValue = x$fixed,
                         obsLevels = x$obsLevels)
            if(length(x$dots) > 0) args <- c(args, x$dots)

            modelObj <- do.call(createModel,
                                args)

            holdBack <-  x$data[-x$index[[i]],]
            observed <- holdBack$.outcome
            holdBack$.outcome <- NULL
            if(any(colnames(holdBack) == ".modelWeights")) holdBack$.modelWeights <- NULL
            
            predicted <- caret:::predictionFunction(x$method,
                                                    modelObj,
                                                    holdBack,
                                                    x$seq)

            if(is.null(x$seq))
              {
                tmp <- data.frame(obs = observed,
                                  pred = predicted,
                                  Resample = paste(x$worker, "R", i, sep = ""))
                if(names(tmp)[2] != "pred") names(tmp)[2] <- "pred"
                tmp <- merge(tmp, x$fixed)
                out <- if(i > 1) rbind(out, tmp) else tmp
              } else {
                ## We want to merge in the model information. For sequential models, this
                ## means merging the fixed and sequential parameters. First we rename the
                ## columns of predicted for merging.
                
                names(predicted) <- paste("mod", 1:ncol(predicted))

                ## param will be a data frame with the appropriate fixed and sequential
                ## factors
                param <- expand(x$fixed, x$seq)
                if(x$method == "ctree") param <- param[!duplicated(param),, drop = FALSE]

                ## For glmnet, we fit all the lambdas but x$fixed has an NA
                if(x$method == "glmnet") param <- param[complete.cases(param),, drop = FALSE]
                
                param$ind <-  paste("mod", 1:ncol(predicted))

                ## Stack the predictions then merge on the model indicators
                tmp <- merge(stack(predicted), param)
                tmp$Resample <- paste(x$resample, i, sep = "")
                tmp$obs <-  rep(observed, ncol(predicted))
                tmp$ind <- NULL
                names(tmp)[names(tmp) == "values"] <- "pred"
                out <- if(i == 1) tmp else rbind(out, tmp)                
              }
            
            rm(tmp)
          }

        if(is.character(observed) | is.factor(observed))
          {
            out$obs <- as.character(out$obs)
            out$pred <- as.character(out$pred)
          }
      }

    out
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

    perf <- lapply(splitUp, func, lev = levels, model = mod)
    perf <- do.call("rbind", perf)

    groupValues <- lapply(splitUp, function(x, y) x[1,y,drop = FALSE], y = groups)
    groupValues <- do.call("rbind", groupValues)

    result1 <- cbind(groupValues, perf)
    result1$parameterGroup <- NULL
    result1$Resample <- NULL

    if(!loo)
      {
        groups <- groups[groups != "Resample"]
        result1$parameterGroup <- factor(
                                         apply(result1[, groups, drop = FALSE],
                                               1,
                                               function(x) paste(x, collapse = "--")))

        splitUp <- split(result1, result1$parameterGroup)

        meanFunc <- function(x, y)
          {
            x <- x[, !(colnames(x) %in% c(y, "parameterGroup")), drop = FALSE]
            mean(x, na.rm = TRUE)
          }
        means <- lapply(splitUp, meanFunc, y = groups)
        means <- do.call("rbind", means)

                sdFunc <- function(x, y)
          {
            x <- x[, !(colnames(x) %in% c(y, "parameterGroup")), drop = FALSE]
            sd(x, na.rm = TRUE)
          }
        sds <- lapply(splitUp, sdFunc, y = groups)
        sds <- do.call("rbind", sds)
        colnames(sds) <- paste( colnames(sds), "SD", sep = "")

        groupValues <- lapply(splitUp, function(x, y) x[1,y,drop = FALSE], y = groups)
        groupValues <- do.call("rbind", groupValues)
        colnames(groupValues) <- gsub("^\\.", "", colnames(groupValues))
        
        out <- cbind(groupValues, means, sds)
        rownames(out) <- 1:nrow(out)
        return(list(values = result1, results = out))
      } else {
        rownames(result1) <- 1:nrow(result1)
        isParam <- colnames(groupValues) %in% groups
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

