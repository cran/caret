
"createModel" <-
  function(data, method, tuneValue, obsLevels, ...)
{

# pam and will crash if there is a resample with <2 observations
# in a class. We will detect this and remove those classes.
  if(method %in% c("pam"))
    {
      yDist <- table(data$.outcome)
      if(any(yDist < 2))
        {
          smallClasses <- names(yDist[yDist < 2])
          data <- data[!(data$.outcome %in% smallClasses),]
        }
    }

  type <- if(is.factor(data$.outcome)) "Classification" else "Regression"

  # whenever possible, instead of using formulas, we will use matrix interfaces to 
  # call models. For data sets with a lot of factors, the terms 
  # object created using formulas gets huge and the same thing can 
                                        # usually be accomplished with the non-formula interface

  modFormula <- as.formula(".outcome ~ .")

  # We refactor the class labels. Some methods bark/crash when there are
  # factor levels that do not have values represented in the data (nnet produces
  # a warning and randomForest throws an error). 
  if(type == "Classification") data$.outcome <- factor(data$.outcome)

  # Later, when we assign predictions, we will convert perdicitons to 
  # character and then create factors from them with levels originally
  # found in the object obsLevels.

  # Some routines dont have formula inputs (or dont handle them well)
  # so extract the feature matrix and class factor.
  # for svm, we can use the formula interface, but we can't save predicted
  # probabilites, so we will use the svm(x, y) interface
  if(method %in% c("glmboost", "blackboost", "gamboost", "earth", "earthTest",
                   "bagFDA", "bagEarth", "lda", "enet", "lasso",
                   "lvq", "pls", "plsTest", "gbm", "pam", "rf",
                   "ada", "knn", "PLS", "rfNWS", "rfLSF", "pcaNNet",
                   "mars", "rda",  "gpls", "svmpoly", "svmradial",
                   "sddaLDA", "sddaQDA", "glmnet"))
    {
      trainX <- data[,!(names(data) %in% ".outcome")]
      trainY <- data[,".outcome"] 
    }
  
  if(method == "gbm" & type == "Classification") 
    gbmClasses <- ifelse(trainY == obsLevels[1], 1, 0)

  xNames <- names(data)[!(names(data) %in% ".outcome")]
  
  modelFit <- switch(method,
                     rda = 
                     {
                       library(klaR)
                       rda(trainX, trainY, gamma = tuneValue$.gamma, lambda = tuneValue$.lambda, ...)
                     },
                     gbm =  
                     {
                       library(gbm)
                       if(type == "Classification")
                         {
                           modY <- gbmClasses
                           modDist <- "bernoulli"
                         } else {
                           modY <- trainY
                           modDist <- "gaussian"
                         }
                       out <- gbm.fit(trainX, modY, interaction.depth = tuneValue$.interaction.depth,
                                      n.trees = tuneValue$.n.trees, shrinkage = tuneValue$.shrinkage, 
                                      distribution = modDist, ...)
                     },
                     rf =
                     {
                       library(randomForest)
                       randomForest(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },
                     rfNWS =
                     {
                       # The following is sneaky. I'm trying to avoid having this package
                       # listed in the description file since it is commercial package and
                       # not on cran.
                       do.call("library", list("randomForestNWS"))
                       randomForestNWS(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },
                     rfLSF =
                     {
                       # See above
                       do.call("library", list("caretLSF"))
                       rfLSF(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },                     
                     svmpoly = 
                     {
                       library(kernlab)
                       if(type == "Classification")
                         {
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = polydot(
                                         degree = tuneValue$.degree,
                                         scale = tuneValue$.scale,
                                         offset = 1),
                                       C = tuneValue$.C,
                                       prob.model = TRUE,
                                       ...)
                         } else out <- ksvm(
                                            as.matrix(trainX),
                                            trainY,
                                            kernel = polydot(
                                              degree = tuneValue$.degree,
                                              scale = tuneValue$.scale,
                                              offset = 1),
                                            C = tuneValue$.C,
                                            ...)
                       out            
                     },
                     svmradial = 
                     {      
                       library(kernlab)      
                       if(type == "Classification")
                         {
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = rbfdot(sigma = tuneValue$.sigma),
                                       C = tuneValue$.C,
                                       prob.model = TRUE,
                                       ...)
                         } else {
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       type = "eps-svr",
                                       kernel = rbfdot(sigma = tuneValue$.sigma),
                                       C = tuneValue$.C,
                                       ...)
                         }
                       out         
                     },                          
                     nnet =
                     {      
                       library(nnet)      
                       nnet(modFormula, data,  size = tuneValue$.size, decay = tuneValue$.decay, ...)
                     },
                     pcaNNet =
                     {      
                       library(nnet)      
                       pcaNNet(trainX, trainY, size = tuneValue$.size, decay = tuneValue$.decay, ...)
                     },                       
                     gpls = 
                     {      
                       library(gpls)      
                       gpls(trainX, trainY, data, K.prov = tuneValue$.K.prov, ...)
                     },         
                     lvq = 
                     {
                       library(class)      
                       lvq3(trainX, trainY, lvqinit(trainX, trainY, k = tuneValue$.k), ...)
                     },         
                     rpart = 
                     {
                       library(rpart)   
                       
                       theDots <- list(...)
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$maxdepth <- tuneValue$.maxdepth
                           theDots$control$xval <- 0 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- rpart.control(maxdepth = tuneValue$.maxdepth, xval = 0)   
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("rpart", modelArgs)
                       out
                     }, 
                     pls =, plsTest = 
                     {

                       library(pls)
                       
                       out <- if(type == "Classification")
                         {      
                           plsda(
                                 trainX, 
                                 trainY,
                                 ncomp = tuneValue$.ncomp, ...)
                         } else {
                           plsr(
                                modFormula,
                                data = data,
                                ncomp = tuneValue$.ncomp, ...)
                         }

                       out
                     },
                     
                     PLS = 
                     {    
                       library(pls)
                       out <- PLS(trainX, trainY, ncomp = tuneValue$.ncomp, ...)
                       out
                     },         
                     pam = 
                     {
                       library(pamr)      
                       pamr.train(list(x = t(trainX), y = trainY), threshold = tuneValue$.threshold, ...)
                     },         
                     knn =
                     {
                       knn3(as.matrix(trainX), trainY, k = tuneValue$.k, ...)
                     },
                     nb =
                     {
                       library(klaR)
                       NaiveBayes(modFormula, data, usekernel= tuneValue$.usekernel, ...)
                     },
                     mars =, earth =, earthTest =
                     {
                       library(earth)
                       
                       theDots <- list(...)
                       theDots$keepxy <- TRUE 
#                       theDots$pmethod <- "none"        
                       
                       modelArgs <- c(
                                      list(
                                           x = trainX,
                                           y = trainY,
                                           degree = tuneValue$.degree,
                                           nprune = tuneValue$.nprune),
                                      theDots)
                       
                       tmp <- do.call("earth", modelArgs)

                       tmp$call["nprune"] <-  tuneValue$.nprune
                       tmp$call["degree"] <-  tuneValue$.degree
                       tmp  
                     },
                     
                     fda =
                     {
                       library(mda)
                       library(earth)
                       fda(modFormula, data, method = earth, 
                           degree = tuneValue$.degree,
                           nprune = tuneValue$.nprune, ...)
                     },
                     
                     bagEarth =
                     {
                       library(earth)
                       bagEarth(trainX, trainY, degree = tuneValue$.degree,
                                nprune = tuneValue$.nprune, ...)
                     },
                     
                     bagFDA =
                     {
                       library(mda)
                       library(earth)
                       bagFDA(modFormula, data, 
                              degree = tuneValue$.degree,
                              nprune = tuneValue$.nprune, ...)
                     },      
                     treebag = 
                     {
                       library(ipred)
                       bagging(modFormula, data, ...)
                     },
                     lm = lm(modFormula, data, ...),
                     lda = 
                     {
                       library(MASS)
                       lda(trainX, trainY, ...)     
                     },
                     multinom = 
                     {
                       library(nnet)
                       multinom(modFormula, data, decay = tuneValue$.decay, ...)     
                     },
                     glmboost = 
                     {
                       library(mboost)
                       
                                        #check for control list and over-write mstop
                       theDots <- list(...)
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$mstop <- tuneValue$.mstop 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- boost_control(mstop = tuneValue$.mstop)
                       
                       if(!any(names(theDots) == "family"))
                         {
                           theDots$family <- if(is.factor(trainY)) Binomial() else GaussReg()              
                         }
                       
                       modelArgs <- c(
                                      list(
                                           x = as.matrix(trainX),
                                           y = trainY,
                                           control = ctl),
                                      theDots)
                       
                       # need to do do.call
                       out <- do.call("glmboost", modelArgs)
                       
                       if(tuneValue$.prune == "yes")
                         {
                           out <- if(is.factor(trainY)) out[mstop(AIC(out, "classical"))] else out[mstop(AIC(out))]
                         }
                       
                       # for easier printing (and tracebacks), we'll try to make the calls shorter
                       # by adding dummy object names instead of the long obkect definitions that
                       # currently exist
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"         
                       
                       out

                     },
                     gamboost = 
                     {
                       library(mboost)
                       
                       #check for control list and over-write mstop
                       theDots <- list(...)
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$mstop <- tuneValue$.mstop 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- boost_control(mstop = tuneValue$.mstop)
                       
                       if(!any(names(theDots) == "family"))
                         {
                           theDots$family <- if(is.factor(trainY)) Binomial() else GaussReg()              
                         }    
                       
                       modelArgs <- c(
                                      list(
                                           x = as.matrix(trainX),
                                           y = trainY,
                                           control = ctl),
                                      theDots)
                       
                        # need to do do.call
                       out <- do.call("gamboost", modelArgs)
                       
                       if(tuneValue$.prune == "yes")
                         {
                           tmp <- if(is.factor(trainY)) try(out[mstop(AIC(out, "classical"))], silent = TRUE) else try(out[mstop(AIC(out))], silent = TRUE)
                         }

                       # for easier printing (and tracebacks), we'll try to make the calls shorter
                       # by adding dummy object names instead of the long obkect definitions that
                        # currently exist
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"  
                       
                       out

                     },      
                     blackboost = 
                     {
                       library(mboost)
                       
                       #check for control lists and over-write mstop and maxdepth
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "tree_controls"))
                         {
                           theDots$tree_controls$maxdepth <- tuneValue$.maxdepth 
                           treeCtl <- theDots$tree_controls
                           theDots$tree_controls <- NULL
                           
                         } else treeCtl <- ctree_control(maxdepth = tuneValue$.maxdepth)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$mstop <- tuneValue$.mstop 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- boost_control(mstop = tuneValue$.mstop)        
                       
                       if(!any(names(theDots) == "family"))
                         {
                           theDots$family <- if(is.factor(trainY)) Binomial() else GaussReg()              
                         }  
                       
                       modelArgs <- c(
                                      list(
                                           x = as.matrix(trainX),
                                           y = trainY,
                                           control = ctl,
                                           tree_controls = treeCtl),
                                      theDots)
                       
                      
                       out <- do.call("blackboost", modelArgs)

                       # for easier printing (and tracebacks), we'll try to make the calls shorter
                       # by adding dummy object names instead of the long obkect definitions that
                       # currently exist
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"  
                       
                       out

                     },
                     
                     ada = 
                     {
                       library(ada)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$maxdepth <- tuneValue$.maxdepth 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- rpart.control(maxdepth = tuneValue$.maxdepth) 
                       
                       modelArgs <- c(
                                      list(
                                           x = trainX,
                                           y = trainY,
                                           iter = tuneValue$.iter,
                                           nu = tuneValue$.nu,              
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("ada", modelArgs)
                       
                       # for easier printing (and tracebacks), we'll try to make the calls shorter
                       # by adding dummy object names instead of the long object definitions that
                       # currently exist
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"  
                       
                       out      
                     },
                     
                     ctree = 
                     {
                       library(party)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$mincriterion <- tuneValue$.mincriterion 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- ctree_control(mincriterion = tuneValue$.mincriterion)          
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("ctree", modelArgs)
                       out        
                     },

                     ctree2 = 
                     {
                       library(party)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$maxdepth <- tuneValue$.maxdepth
                           theDots$control$mincriterion <- 0
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- ctree_control(
                                                     maxdepth = tuneValue$.maxdepth,
                                                     mincriterion = 0)          
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("ctree", modelArgs)
                       out        
                     },                     
                     
                     cforest = 
                     {
                       library(party)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$mtry <- tuneValue$.mtry 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- cforest_control(mtry = tuneValue$.mtry)          
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("cforest", modelArgs)
                       out        
                     },
                     enet =, lasso =
                     {
                       library(elasticnet)
                       lmbda <- if(method == "lasso") 1 else tuneValue$.lambda
                       enet(as.matrix(trainX), trainY, lambda = lmbda) 
                     },
#                     glmnet =
#                     {
#                       library(glmnet)
#                       numLev <- if(is.character(trainY) | is.factor(trainY)) length(levels(trainY)) else NA
#                       if(!is.na(numLev))
#                         {
#                           fam <- ifelse(numLev > 2, "multinomial", "binomial")
#                         } else fam <- "gaussian"
#                       glmnet(as.matrix(x), y, alpha = tuneValue$.alpha, family = fam, ...)
#
#
#                     },
                       
                     sddaLDA = 
                     {
                       library(SDDA)
                       sdda(as.matrix(trainX), trainY, method = "lda", ...)
                     },
                     sddaQDA = 
                     {
                       library(SDDA)
                       sdda(as.matrix(trainX), trainY, method = "qda", ...)
                     }                 
                     )
  
  
  #save a few items so we have a self contained set of information in the model. this will
  # also carry over to the finalModel if returnData = TRUE in train call

  # for models using S4 classes, you can't easily append data, so 
  # exclude these and we'll use other methods to get this information
  if(!(method %in% c("svmradial", "svmpoly", "ctree", "ctree2", "cforest")))
    {
      modelFit$xNames <- xNames
      modelFit$problemType <- type
      modelFit$tuneValue <- tuneValue
      modelFit$obsLevels <- obsLevels
    }

  modelFit
}
