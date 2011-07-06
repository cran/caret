
"createModel" <-
  function(data, method, tuneValue, obsLevels, pp = NULL, ...)
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


  ## We don't want the model weights (if any) to enter the model as predictors
  if(any(colnames(data) == ".modelWeights"))
    {
      modelWeights <- data$.modelWeights
      data$.modelWeights <- NULL
    } else modelWeights <- NULL
  
  ## We refactor the class labels. Some methods bark/crash when there are
  ## factor levels that do not have values represented in the data (nnet produces
  ## a warning and randomForest throws an error). 
  if(type == "Classification") data$.outcome <- factor(as.character(data$.outcome), levels = obsLevels)

  xNames <- names(data)[!(names(data) %in% ".outcome")]
  
  ## Later, when we assign predictions, we will convert predictions to 
  ## character and then create factors from them with levels originally
  ## found in the object obsLevels.

  ## Some routines dont have formula inputs (or dont handle them well)
  ## so extract the feature matrix and class factor.
  if(method %in% c("glmboost", "blackboost", "gamboost", "earth", "earthTest",
                   "bagFDA", "bagEarth", "lda", "enet", "lasso", "nnet", "gcvEarth",
                   "lvq", "pls", "plsTest", "gbm", "pam", "rf", "logitBoost",
                   "ada", "knn", "PLS", "rfNWS", "rfLSF", "pcaNNet",
                   "mars", "rda",  "gpls", "svmpoly", "svmradial", "svmRadialCost",
                   "svmPoly", "svmRadial", "svmLinear", "cubist",
                   "lssvmPoly", "lssvmRadial", "lssvmLinear",
                   "rvmRadial", "rvmPoly", "rvmLinear",
                   "leapForward", "leapBackward", "leapSeq",
                   "gaussprRadial", "gaussprPoly", "gaussprLinear",
                   "sddaLDA", "sddaQDA", "glmnet", "slda", "spls", "smda",
                   "qda", "relaxo", "lars", "lars2", "rlm", "vbmpRadial",
                   "superpc", "ppr", "sda", "penalized", "sparseLDA",
                   "nodeHarvest", "Linda", "QdaCov", "stepLDA", "stepQDA",
                   "parRF", "plr", "rocc", "foba", "partDSA", "hda", "icr", "Boruta",
                   "plsGlmBinomial", "plsGlmGaussian", "plsGlmGamma", "plsGlmPoisson",
                   "bstTree", 'bstLs', 'bstSm',
                   "qrf", "scrda", "bag", "hdda", "logreg", "logforest", "logicBag", "qrnn"))
    {
      trainX <- data[,!(names(data) %in% ".outcome"), drop = FALSE]
      trainY <- data[,".outcome"]
      if(!is.null(pp))
        {
          ppObj <- preProcess(trainX, method = pp$options, thresh = pp$thresh, n.comp = pp$ica, k = pp$k)
          trainX <- predict(ppObj, trainX)
          data <- trainX
          data$.outcome <- trainY
        } else ppObj <- NULL
    } else {
      if(!is.null(pp))
        {
          y <- data$.outcome
          data$.outcome <- NULL
          ppObj <- preProcess(data, method = pp$options, thresh = pp$thresh, n.comp = pp$ica, k = pp$k)
          data <- predict(ppObj, data)
          data$.outcome <- y
        } else ppObj <- NULL
    }
  
  if(method %in% c("gbm", "plr") & type == "Classification") 
    numClasses <- ifelse(trainY == obsLevels[1], 1, 0)
  
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

                       ## check to see if weights were passed in (and availible)
                       if(!is.null(modelWeights)) theDots$w <- modelWeights                      
                       modY <- if(type == "Classification") numClasses else trainY

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
                           useProbModel <- !(any(names(list(...)) == "class.weights"))
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = polydot(
                                         degree = tuneValue$.degree,
                                         scale = tuneValue$.scale,
                                         offset = 1),
                                       C = tuneValue$.C,
                                       prob.model = useProbModel,
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
                           useProbModel <- !(any(names(list(...)) == "class.weights"))
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = rbfdot(sigma = tuneValue$.sigma),
                                       C = tuneValue$.C,
                                       prob.model = useProbModel,
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
                     svmRadialCost = 
                     {      
                       library(kernlab)      
                       if(type == "Classification")
                         {
                           useProbModel <- !(any(names(list(...)) == "class.weights"))
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       C = tuneValue$.C,
                                       prob.model = useProbModel,
                                       ...)
                         } else {
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       C = tuneValue$.C,
                                       ...)
                         }
                       out         
                     },                     
                     svmLinear = 
                     {      
                       library(kernlab)      
                       if(type == "Classification")
                         {
                           useProbModel <- !(any(names(list(...)) == "class.weights"))
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = vanilladot(),
                                       C = tuneValue$.C,
                                       prob.model = useProbModel,
                                       ...)
                         } else {
                           out <- ksvm(
                                       as.matrix(trainX),
                                       trainY,
                                       kernel = vanilladot(),
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
                     rvmLinear = 
                     {      
                       library(kernlab)
                       out <- rvm(
                                  as.matrix(trainX),
                                  trainY,
                                  kernel = vanilladot(),
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
                     lssvmLinear = 
                     {
                       library(kernlab)

                       out <- lssvm(
                                    as.matrix(trainX),
                                    trainY,
                                    kernel = vanilladot(),
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
                     gaussprLinear = 
                     {      
                       library(kernlab)      
                       if(type == "Classification")
                         {
                           out <- gausspr(
                                          as.matrix(trainX),
                                          trainY,
                                          kernel = vanilladot(),
                                          ...)
                         } else {
                           out <- gausspr(
                                          as.matrix(trainX),
                                          trainY,
                                          kernel = vanilladot(),
                                          ...)
                         }
                       out         
                     },                           
                     nnet =
                     {      
                       library(nnet)
                       if(!is.null(modelWeights))
                         {
                           out <- nnet(modFormula,
                                       data = data,
                                       weights = modelWeights,                                       
                                       size = tuneValue$.size,
                                       decay = tuneValue$.decay,
                                       ...)
                         } else out <- nnet(modFormula,
                                            data = data,
                                            size = tuneValue$.size,
                                            decay = tuneValue$.decay,
                                            ...)
                       out
                     },
                     pcaNNet =
                     {
                       ## todo: this needs to be tested
                       library(nnet)
                       if(!is.null(modelWeights))
                         {
                           out <- pcaNNet(modFormula,
                                          data = data,
                                          weights = modelWeights,                                       
                                          size = tuneValue$.size,
                                          decay = tuneValue$.decay,
                                          ...)
                         } else out <- pcaNNet(modFormula,
                                               data = data,
                                               size = tuneValue$.size,
                                               decay = tuneValue$.decay,
                                               ...)
                       out
                     },                       
                     gpls = 
                     {      
                       library(gpls)      
                       gpls(trainX, trainY, data, K.prov = tuneValue$.K.prov, ...)
                     },         
                     lvq = 
                     {
                       library(class)      
                       lvq3(trainX, trainY,
                            lvqinit(trainX, trainY,
                                    size = tuneValue$.size,
                                    k = tuneValue$.k),
                            ...)
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

                       ## check to see if weights were passed in (and availible)
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights    
                       
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

                     pcr = 
                     {

                       library(pls)
                       
                       out <- pcr(modFormula,
                                  data = data,
                                  ncomp = tuneValue$.ncomp, ...)

                       out
                     },                     
                              
                     pam = 
                     {
                       library(pamr)      
                       pamr.train(list(x = t(trainX), y = trainY), threshold = tuneValue$.threshold, ...)
                     },         
                     knn =
                     {
                       if(type == "Classification")
                         {
                           knn3(as.matrix(trainX), trainY, k = tuneValue$.k, ...)
                         } else {
                           knnreg(as.matrix(trainX), trainY, k = tuneValue$.k, ...)
                         }
                     },
                     nb =
                     {
                       library(klaR)
                       NaiveBayes(modFormula, data, usekernel= tuneValue$.usekernel, fL = tuneValue$fL, ...)
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
                       if(type == "Classification") modelArgs$glm <- list(family=binomial)
                       
                       tmp <- do.call("earth", modelArgs)

                       tmp$call["nprune"] <-  tuneValue$.nprune
                       tmp$call["degree"] <-  tuneValue$.degree
                       tmp  
                     },
                     gcvEarth =
                     {
                       library(earth)
                       
                       theDots <- list(...)
                       theDots$keepxy <- TRUE 
                       
                       modelArgs <- c(
                                      list(
                                           x = trainX,
                                           y = trainY,
                                           degree = tuneValue$.degree),
                                      theDots)
                       if(type == "Classification") modelArgs$glm <- list(family=binomial)
                       
                       tmp <- do.call("earth", modelArgs)

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
                     lm =
                     {
                       if(!is.null(modelWeights))
                         {
                           out <- lm(modFormula,
                                     data = data,
                                     weights = modelWeights,
                                     ...)
                         } else out <- lm(modFormula, data, ...)
                       out
                     },
                     lmStepAIC =
                     {
                       library(MASS)
                       if(!is.null(modelWeights))
                         {
                           out <- stepAIC(
                                          lm(modFormula,
                                             data = data,
                                             weights = modelWeights),
                                          ...)
                         } else out <- stepAIC(lm(modFormula, data), ...)
                       out                       
                     },
                     glmStepAIC =
                     {
                       library(MASS)
                       ##check for family in dot and over-write if none
                       theDots <- list(...)
                       if(!any(names(theDots) == "family"))
                         {
                           theDots$family <- if(is.factor(data$.outcome)) binomial() else gaussian()              
                         }

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
                       modelArgs <- c(
                                      list(formula = modFormula,
                                           data = data),
                                      theDots)

                       out <- stepAIC(do.call("glm", modelArgs))
                       out$call <- NULL
                       out
                                
                     },                     
                     lda = 
                     {
                       library(MASS)
                       lda(trainX, trainY, ...)     
                     },
                     multinom = 
                     {
                       library(nnet)
                       if(!is.null(modelWeights))
                         {
                           out <- multinom(modFormula,
                                           data,
                                           weights = modelWeights,                                       
                                           decay = tuneValue$.decay,
                                           ...)
                         } else out <- multinom(modFormula,
                                                data,
                                                decay = tuneValue$.decay,
                                                ...)
                       out    
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

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights                       
                       
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

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights  

                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                      
                       
                       out <- do.call("gamboost", modelArgs)
                       
                       if(tuneValue$.prune == "yes")
                         {
                           tmp <- if(is.factor(trainY)) try(out[mstop(AIC(out, "classical"))], silent = TRUE) else try(out[mstop(AIC(out))], silent = TRUE)
                         }
                       
                       out

                     },      
                     blackboost = 
                     {
                       library(mboost)
                       library(party)
                       
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

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl,
                                           tree_controls = treeCtl),
                                      theDots)                     
                       
                       out <- do.call("blackboost", modelArgs)
                       out$call["data"] <- "data"  
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

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
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
                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
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
                       
                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
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
                       lmbda <- if(method == "lasso") 0 else tuneValue$.lambda
                       enet(as.matrix(trainX), trainY, lambda = lmbda) 
                     },
                     glmnet =
                     {
                       library(glmnet)
                       numLev <- if(is.character(trainY) | is.factor(trainY)) length(levels(trainY)) else NA

                       theDots <- list(...)
                       
                       if(all(names(theDots) != "family"))
                         {
                           if(!is.na(numLev))
                             {
                               fam <- ifelse(numLev > 2, "multinomial", "binomial")
                             } else fam <- "gaussian"
                           
                           theDots$family <- fam   
                         }
                       
                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
                       modelArgs <- c(
                                      list(
                                           x = as.matrix(trainX),
                                           y = trainY,
                                           alpha = tuneValue$.alpha),
                                      theDots)
                       
                       out <- do.call("glmnet", modelArgs) 
                       out 
                     },
                     
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
                           theDots$control$U <- ifelse(tuneValue$.smoothed == "No", TRUE, FALSE)
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(N = ifelse(tuneValue$.pruned == "No", TRUE, FALSE),
                                                    U = ifelse(tuneValue$.smoothed == "No", TRUE, FALSE)) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("M5Rules", modelArgs) 
                       out      
                     },
                     M5 = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$N <- ifelse(tuneValue$.pruned == "No", TRUE, FALSE)
                           theDots$control$U <- ifelse(tuneValue$.smoothed == "No", TRUE, FALSE)
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(N = ifelse(tuneValue$.pruned == "No", TRUE, FALSE),
                                                    U = ifelse(tuneValue$.smoothed == "No", TRUE, FALSE)) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call(if(tuneValue$.rules == "Yes") "M5Rules" else "M5P", modelArgs) 
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
                       if(!is.null(modelWeights))
                         {
                           out <- ppr(as.matrix(trainX),
                                      trainY,
                                      weights = modelWeights,
                                      nterms = tuneValue$.nterms,
                                      ...)
                         } else {
                           out <- ppr(as.matrix(trainX), trainY, nterms = tuneValue$.nterms, ...)
                         }
                       out
                     },
                     sparseLDA =
                     {
                       library(sparseLDA)
                       sparseLDA:::sda(trainX, trainY, lambda = tuneValue$.lambda, stop = -tuneValue$.NumVars, ...)
                     },
                     smda =
                     {
                       library(sparseLDA)
                       smda(trainX, trainY,
                            Rj = tuneValue$.R,
                            lambda = tuneValue$.lambda,
                            stop = -tuneValue$.NumVars,
                            ...)
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
                           caret:::splsda(trainX, trainY, K = tuneValue$.K, eta = tuneValue$.eta,
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

                       ## pass in any model weights
                       if(!is.null(modelWeights)) theDots$weights <- modelWeights
                       
                       modelArgs <- c(
                                      list(formula = modFormula,
                                           data = data),
                                      theDots)
     
                       out <- do.call("glm", modelArgs)
                       out$call <- NULL
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
                       if(!is.null(modelWeights))
                         {
                           out <- fda(modFormula,
                                      data = data,
                                      method = gen.ridge,
                                      weights = modelWeights,
                                      lambda = tuneValue$.lambda,
                                      ...)
                         } else {
                           out <- fda(modFormula,
                                      data = data,
                                      method = gen.ridge,
                                      lambda = tuneValue$.lambda,
                                      ...)
                         }
                       out                    
                     },
                     pda2 =
                     {
                       library(mda)
                       if(!is.null(modelWeights))
                         {
                           out <- fda(modFormula,
                                      data = data,
                                      method = gen.ridge,
                                      weights = modelWeights,
                                      df = tuneValue$.df,
                                      ...)
                         } else {
                           out <- fda(modFormula,
                                      data = data,
                                      method = gen.ridge,
                                      df = tuneValue$.df,
                                      ...)
                         }
                       out                         
                     },                     
                     qda =
                     {
                       library(MASS)
                       qda(trainX, trainY, ...)
                     },
                     relaxo =
                     {
                       library(relaxo)
                       relaxo(as.matrix(trainX), trainY, phi = tuneValue$.phi, ...)
                     },
                     lars =, lars2 =
                     {
                       library(lars)
                       lars(as.matrix(trainX), trainY, ...)
                     },
                     OneR = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           ctl <- theDots$control                        
                         } else ctl <- Weka_control() 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("OneR", modelArgs) 
                       out      
                     },                       
                     PART = 
                     {
                       library(RWeka)
                       
                       theDots <- list(...)
                       
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$U <- ifelse(tuneValue$.pruned == "No", TRUE, FALSE)
                           theDots$control$C <- tuneValue$.threshold
                           ctl <- theDots$control
                           theDots$control <- NULL
                           
                         } else ctl <- Weka_control(N = ifelse(tuneValue$.pruned == "No", TRUE, FALSE),
                                                    C = tuneValue$.threshold) 
                       
                       modelArgs <- c(
                                      list(
                                           formula = modFormula,
                                           data = data,
                                           control = ctl),
                                      theDots)
                       
                       out <- do.call("PART", modelArgs) 
                       out      
                     },
                     rlm =
                     {
                       library(MASS)
                       if(!is.null(modelWeights))
                         {
                           out <- rlm(modFormula, data = data, weights = modelWeights, ...)
                         } else {
                           out <- rlm(modFormula, data = data, ...)
                         }
                       out                        
                     },
                     vbmpRadial =
                     {
                       library(vbmp)
                       theDots <- list(...)
                       if(any(names(theDots) == "control"))
                         {
                           theDots$control$bThetaEstimate <- ifelse(tuneValue$.estimateTheta == "Yes", TRUE, FALSE)
                           ctl <- theDots$control
                           theDots$control <- NULL
                         } else ctl <- list(bThetaEstimate = ifelse(tuneValue$.estimateTheta == "Yes", TRUE, FALSE))                        
                       if(any(names(theDots) == "theta"))
                         {
                           theta <- theDots$theta
                           theDots$theta <- NULL
                         } else theta <- runif(ncol(trainX))

                       vbmp(trainX, as.numeric(trainY),
                            theta = theta,
                            control = ctl,
                            X.TEST = trainX[1,],
                            t.class.TEST  = as.numeric(trainY)[1])
                     },
                     nodeHarvest =
                     {
                       library(nodeHarvest)
                       if(type == "Regression")
                         {
                           out <- nodeHarvest(trainX, trainY,
                                              maxinter = tuneValue$.maxinter,
                                              mode = tuneValue$.mode,
                                              ...)
                         } else {
                           out <- nodeHarvest(trainX,
                                              ifelse(trainY == levels(trainY)[1], 1, 0),
                                              maxinter = tuneValue$.maxinter,
                                              mode = tuneValue$.mode,
                                              ...)                          
                         }
                       out                        
                       },
                     Linda =
                     {
                       library(rrcov)
                       Linda(trainX, trainY, ...)
                     },
                     QdaCov =
                     {
                       library(rrcov)
                       QdaCov(trainX, trainY, ...)
                     },
                     stepLDA =
                     {
                       library(klaR)
                       library(MASS)
                       out <- stepclass(trainX, trainY,
                                 method = "lda",
                                 maxvar = tuneValue$.maxvar,
                                 direction = as.character(tuneValue$.direction),
                                 ...)
                       out$fit <- lda(trainX[, predictors(out), drop = FALSE],
                                      trainY,
                                      ...)
                       out
                     },
                     stepQDA =
                     {
                       library(klaR)
                       library(MASS)
                       out <- stepclass(trainX, trainY,
                                 method = "qda",
                                 maxvar = tuneValue$.maxvar,
                                 direction = as.character(tuneValue$.direction),
                                 ...)
                       out$fit <- qda(trainX[, predictors(out), drop = FALSE],
                                      trainY,
                                      ...)
                       out
                     },
                     parRF =
                     {
                       library(foreach)
                       library(randomForest)
                       
                       workers <- getDoParWorkers()
                       
                       theDots <- list(...)

                       theDots$ntree <- if(is.null(theDots$ntree)) 250 else theDots$ntree

                       theDots$x <- trainX
                       theDots$y <- trainY
                       theDots$mtry <- tuneValue$.mtry
                       theDots$ntree <- ceiling(theDots$ntree/workers)                       
                   
                       out <- foreach(ntree = 1:workers, .combine = combine) %dopar% {
                         library(randomForest)
                         do.call("randomForest", theDots)
                       }
                       out$call["x"] <- "x"
                       out$call["y"] <- "y"
                       out
                     },
                     plr =
                     {
                       library(stepPlr)
                       out <- plr(trainX, numClasses,
                                  lambda = tuneValue$.lambda,
                                  cp = as.character(tuneValue$.cp),
                                  ...)
                       out
                     },
                     GAMens =
                     {
                       library(GAMens)
                       modParam <- list(formula = modFormula,
                                        data = data,
                                        autoform = TRUE,
                                        rsm_size =tuneValue$.rsm_size,
                                        rsm = ifelse(tuneValue$.rsm_size > 0, TRUE, FALSE),
                                        iter = tuneValue$.iter,
                                        bagging = ifelse(tuneValue$.iter > 0, TRUE, FALSE),
                                        fusion = tuneValue$.fusion)
                       theDots <- list(...)
                       if(any(names(theDots) == "autoform"))
                         {
                           warning("autoform is automatically set to TRUE by train()")
                           theDots$autoform <- NULL
                         }
                       if(any(names(theDots) == "rsm"))
                         {
                           warning("rsm is automatically set using the rsm_size parameter by train()")
                           theDots$rsm <- NULL
                         }
                       if(any(names(theDots) == "bagging"))
                         {
                           warning("bagging is automatically set using the rsm_size parameter by train()")
                           theDots$bagging <- NULL
                         }
                       modParam <- c(modParam, theDots)
                       out <- do.call("GAMens", modParam)
                       ## "fix" call afterwords?
                       out  
                     },
                     rocc =
                     {
                       library(rocc)
                       newY <- factor(ifelse(trainY == obsLevels[1], 1, 0), levels = c("0", "1"))
                       tr.rocc(g = t(trainX), out = newY, xgenes = tuneValue$.xgenes)
                     },
                     foba =
                     {
                       library(foba)
                       foba(as.matrix(trainX), trainY,
                            lambda = tuneValue$.lambda,
                            ...)
                     },
                     partDSA =
                     {
                       ## todo better parsing of ... args between func and control
                       library(partDSA)
                       partDSA(trainX, trainY,
                               control = DSA.control(
                                 cut.off.growth = tuneValue$.cut.off.growth,
                                 MPD = tuneValue$.MPD,
                                 vfold = 1),
                               ...)
                     },
                     hda =
                     {
                       library(hda)
                       hda(trainX, trainY,
                           newdim = tuneValue$.newdim,
                           reg.lamb = tuneValue$.lambda,
                           reg.gamm = tuneValue$.gamma,
                           crule = TRUE, ...)
                     },
                     icr =
                     {
                       icr(trainX, trainY,
                           n.comp = tuneValue$.n.comp,
                           ...)
                     },
                     neuralnet =
                     {
                       library(neuralnet)
                       colNames <- colnames(data)
                       colNames <- colNames[colNames != ".outcome"]
                       form <- as.formula(
                                          paste(".outcome ~",
                                                paste(colNames, collapse = "+")))
                       if(tuneValue$.layer1 == 0) stop("the first layer must have at least one hidden unit")
                       if(tuneValue$.layer2 == 0 & tuneValue$.layer2 > 0) stop("the second layer must have at least one hidden unit if a third layer is specified")
                       nodes <- c(tuneValue$.layer1)
                       if(tuneValue$.layer2 > 0)
                         {
                           nodes <- c(nodes, tuneValue$.layer2)
                           if(tuneValue$.layer3 > 0) nodes <- c(nodes, tuneValue$.layer3)
                         }
                       
                       neuralnet(form,
                                 data = data,
                                 hidden = nodes,
                                 ...)
                     },
                     qrf =
                     {
                       library(quantregForest)
                       quantregForest(trainX, trainY, mtry = tuneValue$.mtry, ...)
                     },
                     scrda =
                     {
                       library(rda)
                       modelFit <- rda:::rda(t(trainX), as.numeric(trainY),
                                             alpha = tuneValue$.alpha,
                                             delta = tuneValue$.delta,
                                             ...)
                       modelFit$data <- list(x = t(trainX), y = trainY)
                       modelFit
                     },
                     bag =
                     {
                       bag(trainX, trainY, vars = tuneValue$.vars, ...)
                     },
                     hdda =
                     {
                       library(HDclassif)
                       hdda(trainX, trainY, model = tuneValue$.model, threshold = tuneValue$.threshold, ...)
                     },
                     logreg =
                     {
                       library(LogicReg)
                       if(is.factor(trainY)) trainY <- ifelse(trainY == levels(trainY)[1], 1, 0)
                       logreg(resp = trainY, bin = trainX,
                              ntrees = tuneValue$.ntrees,
                              tree.control = logreg.tree.control(treesize = tuneValue$.treesize),
                              select = 1,
                              type = ifelse(type == "Regression", 2, 3),
                              ...)
                     },
                     logforest =
                     {
                       library(LogicForest)
                       y2 <- ifelse(trainY == levels(trainY)[1], 1, 0)
                       logforest(resp = y2,
                                 Xs = trainX,
                                 ...)
                     },
                     logicBag =
                     {
                       library(logicFS)
                       logic.bagging(as.matrix(trainX), trainY,
                                     ntrees = tuneValue$.ntrees,
                                     nleaves = tuneValue$.nleaves,
                                     ...)
                     },
                     gam =
                     {
                       library(mgcv)
                       mgcv:::gam(gamFormula(data[,!(names(data) %in% ".outcome"), drop = FALSE]),
                                  data = data,
                                  family = if(type == "Regression") gaussian() else  binomial(),
                                  select = tuneValue$.select,
                                  method = tuneValue$.method,
                                  ...)
                     },
                     gamLoess =
                     {
                       library(gam)
                       gam:::gam(gamFormula(data[,!(names(data) %in% ".outcome"), drop = FALSE],
                                            smoother = "lo",
                                            span = tuneValue$.span,
                                            degree = tuneValue$.degree),
                                 data = data,
                                 family = if(type == "Regression") gaussian() else  binomial(),
                                 ...)
                                 
                     },
                     gamSpline =
                     {
                       library(gam)
                       gam:::gam(gamFormula(data[,!(names(data) %in% ".outcome"), drop = FALSE],
                                            smoother = "s",
                                            df = tuneValue$.df),
                                 data = data,
                                 family = if(type == "Regression") gaussian() else  binomial(),
                                 ...)
                                 
                     },
#                     plsGlmBinomial =, plsGlmGaussian =, plsGlmGamma =, plsGlmPoisson =
#                     {
#                       library(plsRglm)
#                       modType <- switch(method,
#                                         plsGlmBinomial = "pls-glm-logistic",
#                                         plsGlmGaussian = "pls-glm-gaussian",
#                                         plsGlmGamma = "pls-glm-Gamma",
#                                         plsGlmPoisson = "pls-glm-poisson")
#                       if(method == "plsGlmBinomial") trainY <- ifelse(trainY == levels(trainY)[1], 1, 0)
#                       plsRglm(trainY, trainX, nt = tuneValue$.nt, modele = modType, ...)
 #                    },
                     qrnn =
                     {
                       library(qrnn)

                       qrnn.fit(as.matrix(trainX), matrix(trainY),
                                n.hidden = tuneValue$.n.hidden,
                                print.level = 0,
                                penalty =  tuneValue$.penalty,
                                bag= tuneValue$.bag,
                                ...)

                     },
                     Boruta =
                     {
                       library(Boruta)
                       library(randomForest)
                       fs <- Boruta(trainX, trainY, mtry = tuneValue$.mtry, ...)
                       keepers <- as.character(names(fs$finalDecision)[fs$finalDecision == "Confirmed"])
                       out <- randomForest(trainX[,keepers, drop = FALSE], trainY, mtry = tuneValue$.mtry, ...)
                       out$Boruta <- fs
                       out
                     },
                     cubist =
                     {
                       library(Cubist)
                       cubist(trainX, trainY,
                              committees =  tuneValue$.committees,
                              ...)
                     },
                     bstTree =  
                     {
                       library(bst)
                       theDots <- list(...)
                       modDist <- if(type == "Classification") "hinge" else "gaussian"
  
                       modY <- if(type == "Classification") ifelse(trainY == obsLevels[1], 1, -1) else trainY

                       if(any(names(theDots) == "ctrl"))
                         {
                           theDots$ctrl$mstop <- tuneValue$.mstop
                           theDots$ctrl$nu <- tuneValue$.nu
                         } else {
                           theDots$ctrl <- bst_control(mstop = tuneValue$.mstop, nu = tuneValue$.nu)
                         }
                       if(any(names(theDots) == "control.tree"))
                         {
                           theDots$control.tree$maxdepth <- tuneValue$.maxdepth
                         } else {
                           theDots$control.tree <- list(maxdepth = tuneValue$.maxdepth)
                         }

                       
                       modArgs <- list(x = trainX,
                                       y = modY,
                                       family = modDist)
                       modArgs <- c(modArgs, theDots)
                       
                       do.call("bst", modArgs)
                     },
                     bstLs =, bstSm =   
                     {
                       library(bst)
                       theDots <- list(...)
                       modDist <- if(type == "Classification") "hinge" else "gaussian"
  
                       modY <- if(type == "Classification") ifelse(trainY == obsLevels[1], 1, -1) else trainY

                       if(any(names(theDots) == "ctrl"))
                         {
                           theDots$ctrl$mstop <- tuneValue$.mstop
                           theDots$ctrl$nu <- tuneValue$.nu
                         } else {
                           theDots$ctrl <- bst_control(mstop = tuneValue$.mstop, nu = tuneValue$.nu)
                         }
                       
                       modArgs <- list(x = trainX,
                                       y = modY,
                                       family = modDist)
                       modArgs <- c(modArgs, theDots)
                       
                       do.call("bst", modArgs)
                     },
                     leapForward =, leapBackward =, leapSeq =
                     {
                       library(leaps)
                       ## check for options
                       theDots <- list(...)
                       if(any(names(theDots) == "nbest")) stop("'nbest' should not be specified")
                       if(any(names(theDots) == "method")) stop("'method' should not be specified")
                       if(any(names(theDots) == "nvmax")) stop("'nvmax' should not be specified")

                       dir <- switch(method,
                                     leapForward = "forward",
                                     leapBackward = "backward",
                                     leapSeq = "seqrep")
                       
                       regsubsets(trainX, trainY,
                                  weights = if(!is.null(modelWeights)) modelWeights else rep(1, length(trainY)),
                                  nbest = 1, nvmax = tuneValue$.nvmax, method = dir, ...)
                     }
                     )
  

  ## In case the model needs to be saved to the file system and run later, we will
  ## need to cache the model (per Kurt Hornik on 2008-10-05)
  ## No ,onger needed?
  ## if(method %in% c("JRip", "LMT", "M5Rules", "J48", "OneR", "PART")) .jcache(modelFit$classifier)
  
  ##save a few items so we have a self contained set of information in the model. this will
  ## also carry over to the finalModel if returnData = TRUE in train call

  ## for models using S4 classes, you can't easily append data, so 
  ## exclude these and we'll use other methods to get this information
  if(!(tolower(method) %in% tolower(c("svmRadial", "svmPoly", "svmLinear", "svmRadialCost",
                                      "rvmRadial", "rvmPoly", "rvmLinear",
                                      "lssvmRadial", "lssvmPoly", "lssvmLinear",
                                      "gaussprRadial", "gaussprPoly", "gaussprLinear",
                                      "ctree", "ctree2", "cforest",
                                      "penalized", "Linda", "QdaCov"))))
    {
      modelFit$xNames <- xNames
      modelFit$problemType <- type
      modelFit$tuneValue <- tuneValue
      modelFit$obsLevels <- obsLevels
    }

  list(fit = modelFit, preProc = ppObj)
}
