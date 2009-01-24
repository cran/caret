
"createModel" <-
  function(data, method, tuneValue, obsLevels, ...)
{

  ## pam and will crash if there is a resample with <2 observations
  ## in a class. We will detect this and remove those classes.
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

  ## whenever possible, instead of using formulas, we will use matrix interfaces to 
  ## call models. For data sets with a lot of factors, the terms 
  ## object created using formulas gets huge and the same thing can 
  ## usually be accomplished with the non-formula interface

  modFormula <- as.formula(".outcome ~ .")

  ## We refactor the class labels. Some methods bark/crash when there are
  ## factor levels that do not have values represented in the data (nnet produces
  ## a warning and randomForest throws an error). 
  if(type == "Classification") data$.outcome <- factor(as.character(data$.outcome), levels = obsLevels)

  ## Later, when we assign predictions, we will convert predictions to 
  ## character and then create factors from them with levels originally
  ## found in the object obsLevels.

  ## Some routines dont have formula inputs (or dont handle them well)
  ## so extract the feature matrix and class factor.
  if(method %in% c("glmboost", "blackboost", "gamboost", "earth", "earthTest",
                   "bagFDA", "bagEarth", "lda", "enet", "lasso",
                   "lvq", "pls", "plsTest", "gbm", "pam", "rf", "logitBoost",
                   "ada", "knn", "PLS", "rfNWS", "rfLSF", "pcaNNet",
                   "mars", "rda",  "gpls", "svmpoly", "svmradial",
                   "svmPoly", "svmRadial",
                   "lssvmPoly", "lssvmRadial",
                   "rvmRadial", "rvmPoly",
                   "gaussprRadial", "gaussprPoly",
                   "sddaLDA", "sddaQDA", "glmnet", "slda", "spls", 
                   "qda",
                   "superpc", "ppr", "sda", "penalized", "sparseLDA"))
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
                       ## train will figure out whether we are doing classification or reggression
                       ## from the class of the outcome and automatically specify the value of
                       ## 'distribution' in the control file. If the user wants to over-ride this,
                       ## this next bit will allow this.
                       theDots <- list(...)
                       if(any(names(theDots) == "distribution"))
                         {
                           modDist <- theDots$distribution
                           theDots$distribution <- NULL
                         } else {
                           modDist <- if(type == "Classification") "bernoulli" else "gaussian"
                         }
                       
                       modY <- if(type == "Classification") gbmClasses else trainY

                       modArgs <- list(x = trainX,
                                       y = modY,
                                       interaction.depth = tuneValue$.interaction.depth,
                                       n.trees = tuneValue$.n.trees,
                                       shrinkage = tuneValue$.shrinkage, 
                                       distribution = modDist)
                       if(length(theDots) > 0) modArgs <- c(modArgs, theDots)
                       
                       do.call("gbm.fit", modArgs)
                     },
                     rf =
                     {
                       library(randomForest)
                       randomForest(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },
                     rfNWS =
                     {
                       ## The following is sneaky. I'm trying to avoid having this package
                       ## listed in the description file since it is commercial package and
                       ## not on cran.
                       do.call("library", list("randomForestNWS"))
                       randomForestNWS(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },
                     rfLSF =
                     {
                       ## See above
                       do.call("library", list("caretLSF"))
                       rfLSF(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },                     
                     svmpoly =, svmPoly = 
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
                     svmradial =, svmRadial = 
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
                                       kernel = rbfdot(sigma = tuneValue$.sigma),
                                       C = tuneValue$.C,
                                       ...)
                         }
                       out         
                     },
                     rvmPoly = 
                     {
                       library(kernlab)
                       ## As of version 0.9-5 of kernlab, there was a small inconsistency
                       ## between methods on how to specify kernels. Unlike ksvm, we specify
                       ## them here via kpar (same for polynomial kernels)

                       out <- rvm(
                                  as.matrix(trainX),
                                  trainY,
                                  kernel = polydot,
                                  kpar = list(
                                    degree = tuneValue$.degree,
                                    scale = tuneValue$.scale,
                                    offset = 1),
                                  ...)
                       out            
                     },
                     rvmRadial = 
                     {      
                       library(kernlab)      

                       out <- rvm(
                                  as.matrix(trainX),
                                  trainY,
                                  kernel = rbfdot,
                                  kpar = list(sigma = tuneValue$.sigma),
                                  ...)

                       out         
                     },
                     lssvmPoly = 
                     {
                       library(kernlab)

                       out <- lssvm(
                                    as.matrix(trainX),
                                    trainY,
                                    kernel = polydot(
                                      degree = tuneValue$.degree,
                                      scale = tuneValue$.scale,
                                      offset = 1),
                                    ...)

                       out            
                     },
                     lssvmRadial = 
                     {      
                       library(kernlab)      

                       out <- lssvm(
                                    as.matrix(trainX),
                                    trainY,
                                    kernel = rbfdot(sigma = tuneValue$.sigma),
                                    ...)

                       out         
                     },
                     gaussprPoly = 
                     {
                       library(kernlab)
                       if(type == "Classification")
                         {
                           out <- gausspr(
                                          as.matrix(trainX),
                                          trainY,
                                          kernel = polydot(
                                            degree = tuneValue$.degree,
                                            scale = tuneValue$.scale,
                                            offset = 1),
                                          ...)
                         } else out <- gausspr(
                                               as.matrix(trainX),
                                               trainY,
                                               kernel = polydot(
                                                 degree = tuneValue$.degree,
                                                 scale = tuneValue$.scale,
                                                 offset = 1),
                                               ...)
                       out            
                     },
                     gaussprRadial = 
                     {      
                       library(kernlab)      
                       if(type == "Classification")
                         {
                           out <- gausspr(
                                          as.matrix(trainX),
                                          trainY,
                                          kernel = rbfdot(sigma = tuneValue$.sigma),
                                          ...)
                         } else {
                           out <- gausspr(
                                          as.matrix(trainX),
                                          trainY,
                                          kernel = rbfdot(sigma = tuneValue$.sigma),
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
                     lmStepAIC =
                     {
                       library(MASS)
                       stepAIC(lm(modFormula, data), ...)
                     },
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
                       
                       ##check for control list and over-write mstop
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
                       
                       out <- do.call("glmboost", modelArgs)
                       
                       if(tuneValue$.prune == "yes")
                         {
                           out <- if(is.factor(trainY)) out[mstop(AIC(out, "classical"))] else out[mstop(AIC(out))]
                         }
                       
                       ## for easier printing (and tracebacks), we'll try to make the calls shorter
                       ## by adding dummy object names instead of the long obkect definitions that
                       ## currently exist
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"         
                       
                       out

                     },
                     gamboost = 
                     {
                       library(mboost)
                       
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
                       
                       
                       out <- do.call("gamboost", modelArgs)
                       
                       if(tuneValue$.prune == "yes")
                         {
                           tmp <- if(is.factor(trainY)) try(out[mstop(AIC(out, "classical"))], silent = TRUE) else try(out[mstop(AIC(out))], silent = TRUE)
                         }
                       
                       out$call["x"] <- "xData"         
                       out$call["y"] <- "yData"  
                       
                       out

                     },      
                     blackboost = 
                     {
                       library(mboost)
                       
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
                           theDots$control@gtctrl@mincriterion <- tuneValue$.mincriterion 
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
                           theDots$control@tgctrl@maxdepth <- tuneValue$.maxdepth
                           theDots$control@gtctrl@mincriterion <- 0
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
                           theDots$control@gtctrl@mtry <- as.integer(tuneValue$.mtry) 
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
                     ##                     glmnet =
                     ##                     {
                     ##                       library(glmnet)
                     ##                       numLev <- if(is.character(trainY) | is.factor(trainY)) length(levels(trainY)) else NA
                     ##                       if(!is.na(numLev))
                     ##                         {
                     ##                           fam <- ifelse(numLev > 2, "multinomial", "binomial")
                     ##                         } else fam <- "gaussian"
                     ##                       glmnet(as.matrix(x), y, alpha = tuneValue$.alpha, family = fam, ...)
                     ##
                     ##
                     ##                     },
                     
                     sddaLDA = 
                     {
                       library(SDDA)
                       sdda(as.matrix(trainX), trainY, method = "lda", ...)
                     },
                     sddaQDA = 
                     {
                       library(SDDA)
                       sdda(as.matrix(trainX), trainY, method = "qda", ...)
                     },
                     logitBoost =
                     {
                       library(caTools)
                       caTools::LogitBoost(as.matrix(trainX), trainY, nIter = tuneValue$.nIter)
                     },
                     J48 = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$C <- tuneValue$.C 
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(C = tuneValue$.C) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("J48", modelArgs) 
                       out      
                     },
                     M5Rules = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$N <- ifelse(tuneValue$.pruned == "No", TRUE, FALSE)
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(N = ifelse(tuneValue$.pruned == "No", TRUE, FALSE)) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("M5Rules", modelArgs) 
                       out      
                     },
                     LMT = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$I <- tuneValue$.iter
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(I = tuneValue$.iter) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("LMT", modelArgs) 
                       out      
                     },  
                     JRip = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$N <- tuneValue$.NumOpt
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(N = tuneValue$.NumOpt) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("JRip", modelArgs) 
                       out      
                     },
                     slda = 
                     {
                       library(ipred)
                       slda(modFormula, data, ...)
                     },
                     superpc = 
                     {
                       library(superpc)      
                       out <- superpc.train(list(x = t(trainX), y = trainY),
                                            type = "regression",
                                            ...)
                       ## prediction will need to source data, so save that too
                       out$data <- list(x = t(trainX), y = trainY)
                       out
                     },
                     ppr =
                     {
                       library(stats)
                       ppr(as.matrix(trainX), trainY, nterms = tuneValue$.nterms, ...)
                     },
                     sparseLDA =
                     {
                       library(sparseLDA)
                       sparseLDA:::sda(trainX, trainY, lambda = tuneValue$.lambda, stop = -tuneValue$.NumVars, ...)
                     },
                     sda =
                     {
                       library(sda)
                       if(!is.matrix(trainX)) trainX <- as.matrix(trainX)
                       sda::sda(trainX, trainY, diagonal = tuneValue$.diagonal, ...)
                     },
                     penalized =
                     {
                       library(penalized)
                       modType <- if(is.factor(trainY)) "logistic" else "linear"
                       penalized(trainY, trainX,
                                 model = modType,
                                 lambda1 = tuneValue$.lambda1,
                                 lambda2 = tuneValue$.lambda2,
                                 ...)
                     },
                     spls =
                     {
                       library(spls)
                       if(is.factor(trainY))
                         {
                           splsda(trainX, trainY, K = tuneValue$.K, eta = tuneValue$.eta,
                                  kappa = tuneValue$.kappa, ...)
                         } else {
                           spls(trainX, trainY, K = tuneValue$.K, eta = tuneValue$.eta,
                                kappa = tuneValue$.kappa, ...)
                         }
                     },
                     glm = 
                     {
                       ##check for family in dot and over-write if none
                       theDots <- list(...)
                       if(!any(names(theDots) == "family"))
                         {
                           theDots$family <- if(is.factor(data$.outcome)) binomial() else gaussian()              
                         }
                       
                       modelArgs <- c(
                                      list(formula = modFormula,
                                           data = data),
                                      theDots)
                       
                       out <- do.call("glm", modelArgs)
                       out
                     },
                     mda =
                     {
                       library(mda)
                       mda(modFormula, data = data, subclasses = tuneValue$.subclasses, ...)
                     },
                     pda =
                     {
                       library(mda)
                       fda(modFormula, data = data, method = gen.ridge, lambda = tuneValue$.lambda, ...)
                     },
                     pda2 =
                     {
                       library(mda)
                       fda(modFormula, data = data, method = gen.ridge, df = tuneValue$.df, ...)
                     },                     
                     qda =
                     {
                       library(MASS)
                       qda(trainX, trainY, ...)
                     }              
                     )
  

  ## In case the model needs to be saved to the file system and run later, we will
  ## need to cache the model (per Kurt Hornik on 2008-10-05)
  if(method %in% c("JRip", "LMT", "M5Rules", "J48")) .jcache(modelFit$classifier)
  
  ##save a few items so we have a self contained set of information in the model. this will
  ## also carry over to the finalModel if returnData = TRUE in train call

  ## for models using S4 classes, you can't easily append data, so 
  ## exclude these and we'll use other methods to get this information
  if(!(tolower(method) %in% tolower(c("svmRadial", "svmPoly",
                                      "rvmRadial", "rvmPoly",
                                      "lssvmRadial", "lssvmPoly",
                                      "gaussprRadial", "gaussprPoly",
                                      "ctree", "ctree2", "cforest",
                                      "penalized"))))
    {
      modelFit$xNames <- xNames
      modelFit$problemType <- type
      modelFit$tuneValue <- tuneValue
      modelFit$obsLevels <- obsLevels
    }

  modelFit
}
