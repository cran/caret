predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL, custom = NULL)
{
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL

  coerceChar <- function(x)  as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)


  if(!is.null(preProc)) newdata <- predict(preProc, newdata)
  
  predictedValue <- switch(method,
                           lda =, rda =, gpls =, slda =, qda =, rrlda = 
                           {
                             switch(method,
                                    lda =, qda = library(MASS),
                                    rda        = library(klaR),
                                    gpls       = library(gpls),
                                    rrlda      = library(rrlda),
                                    slda       = library(ipred))
                             out <- as.character(predict(modelFit, newdata)$class)
                             out
                           },
                           
                           gbm =
                           {
                             library(gbm)
                             if(modelFit$problemType == "Classification")
                               {
                                 gbmProb <- predict(modelFit, newdata, type = "response",
                                                    n.trees = modelFit$tuneValue$.n.trees)
                                 out <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                 ## to correspond to gbmClasses definition above
                               } else {
                                 out <- predict(modelFit, newdata, type = "response",
                                                n.trees = modelFit$tuneValue$.n.trees)
                               }
                             
                             if(!is.null(param))
                               {
                                 preds <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees)
                                 
                                 if(modelFit$problemType == "Classification")
                                   {
                                     preds <- ifelse(preds >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                   }
                                 out <- c(list(out), as.list(as.data.frame(preds)))
                                 out <- if(modelFit$problemType == "Classification") lapply(out, as.character) else out
                               }
                             out
                           },
                           
                           rf =, parRF =, Boruta = 
                           {
                             library(randomForest)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <-  as.character(predict(modelFit, newdata))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             out
                           },
                           
                           svmradial =, svmpoly =,
                           svmRadial =, svmPoly =, svmLinear =,
                           rvmRadial =, rvmPoly =, rvmLinear =,
                           lssvmRadial =, lssvmPoly =, lssvmLinear =,
                           gaussprRadial =, gaussprPoly =, gaussprLinear =,
                           svmRadialCost =
                           {
                             library(kernlab)
                             out <- try(predict(modelFit, newdata), silent = TRUE)
                             if(runif(1) < .1) class(out) <-  "try-error"
                             if(is.character(lev(modelFit)))
                               {
                                 if(class(out)[1] != "try-error")
                                   {
                                     predClass <- as.character(out)
                                     out <- factor(predClass, levels = lev(modelFit))
                                   } else {
                                     warning("kernlab class prediction calculations failed; returning NAs")
                                     out <- rep("", nrow(newdata))
                                     out[seq(along = out)] <- NA
                                   }
                               } else {
                                 if(class(out)[1] == "try-error")
                                   {
                                     warning("kernlab prediction calculations failed; returning NAs")
                                     out <- rep(NA, nrow(newdata))
                                   } 
                               }
                             out
                           },
                           
                           knn =
                           {
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata, type="class"))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             out
                           },
                           
                           avNNet =, nnet =, multinom =, pcaNNet =
                           {
                             library(nnet)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata, type="class"))
                               } else {
                                 out  <- predict(modelFit, newdata, type="raw")
                               }
                             out
                           },
                           
                           rpart2 =
                           {
                             library(rpart)
                             
                             if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata, type="class"))
                               } else {
                                 out  <- predict(modelFit, newdata, type="vector")

                               }

                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 cpValues <- depth2cp(modelFit$cptable, param$.maxdepth)
                                 
                                 for(j in seq(along = cpValues))
                                   {
                                     prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[[j+1]] <- as.character(predict(prunedFit, newdata, type="class"))
                                       } else {
                                         tmp[[j+1]]  <- predict(prunedFit, newdata, type="vector")
                                       }
                                   }
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                             
                           },

                           rpart =
                           {
                             library(rpart)
                             
                             if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata, type="class"))
                               } else {
                                 out  <- predict(modelFit, newdata, type="vector")

                               }

                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                
                                 for(j in seq(along = param$.cp))
                                   {
                                     prunedFit <- prune.rpart(modelFit, cp = param$.cp[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[[j+1]] <- as.character(predict(prunedFit, newdata, type="class"))
                                       } else {
                                         tmp[[j+1]]  <- predict(prunedFit, newdata, type="vector")
                                       }
                                   }
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                             
                           },
                           
                           lda2 = 
                           {
                             library(MASS)
                             out <- as.character(predict(modelFit, newdata, dimen = modelFit$tuneValue$.dimen)$class)
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 for(j in seq(along = param$.dimen))
                                   {
                                     tmp[[j+1]] <- as.character(predict(modelFit, newdata, dimen = param$.dimen[j])$class)
                                   }
                                 out <- tmp
                               }                        
                             out
                           },
                           
                           lvq =
                           {
                             library(class)
                             out <- as.character(lvqtest(modelFit , newdata))
                             out
                           },

                           pcr=, pls =, simpls =, widekernelpls =, kernelpls = 
                           {
                             library(pls)
                             
                             out <- if(modelFit$problemType == "Classification")
                               {
                                 if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                                 out <- predict(modelFit, newdata, type="class")
                                 
                               } else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))

                             if(!is.null(param))
                               {
                                 ## We'll merge the first set of predictions below
                                 tmp <- vector(mode = "list", length = nrow(param))
                                 
                                 if(modelFit$problemType == "Classification")
                                   {
                                     if(length(param$.ncomp) > 1)
                                       {
                                         tmp <- as.list(predict(modelFit, newdata, ncomp = param$.ncomp))
                                       } else tmp <- list( predict(modelFit, newdata, ncomp = param$.ncomp))

                                   } else {
                                     tmp <- as.list(
                                                    as.data.frame(
                                                                  apply(predict(modelFit, newdata, ncomp = param$.ncomp),
                                                                        3,
                                                                        function(x) list(x))))
                                   }

                                 out <- c(list(out), tmp)
                                 if(modelFit$problemType == "Classification") out <- lapply(out, as.character)
                               }
                             out
                           },
                      

                           pam =
                           {
                             library(pamr)
                             
                             out <- as.character(
                                                 pamr.predict(modelFit,
                                                              t(newdata),
                                                              threshold = modelFit$tuneValue$.threshold))
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 for(j in seq(along = param$.threshold))
                                   {
                                     tmp[[j+1]] <- as.character(
                                                                pamr.predict(
                                                                             modelFit,
                                                                             t(newdata),
                                                                             threshold = param$.threshold[j]))
                                   }
                                 out <- tmp
                               }
                             out
                           },
                           
                           nb =
                           {
                             library(klaR)
                             if(is.vector(newdata)) newdata <- as.data.frame(newdata)
                             out <- as.character(predict(modelFit , newdata)$class)
                             out
                           },
                           
                           fda =
                           {
                             library(mda)
                             library(earth)
                             out <- as.character(predict(modelFit , newdata))
                             out
                           },
                           
                           bagFDA =
                           {
                             library(mda)
                             library(earth)
                             out <- as.character(predict(modelFit , newdata))
                             out
                           },
                           
                           treebag =
                           {
                             library(ipred)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata,  type = "class"))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             out
                           },

                           mars =, earth =
                           {
                             library(earth)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata,  type = "class"))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- if(is.matrix(out)) out[,1] else out
                                 
                                 for(j in seq(along = param$.nprune))
                                   {
                                     prunedFit <- update(modelFit, nprune = param$.nprune[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[[j+1]]  <-  as.character(predict(prunedFit, newdata,  type = "class"))
                                       } else {
                                         tmp[[j+1]]  <-  predict(prunedFit, newdata)[,1]
                                       }
                                   }
                                 
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                           },
                           cubist =
                           {
                             library(Cubist)
                             
                             out <- predict(modelFit, newdata, neighbors = modelFit$tuneValue$.neighbors)
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.neighbors))
                                   {
                                     tmp[[j+1]] <- predict(modelFit, newdata, neighbors = param$.neighbors[j])
                                   }
                                 out <- tmp
                               }
                             out
                           },                           
                           
                           gcvEarth =
                           {
                             library(earth)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata,  type = "class"))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             if(is.matrix(out)) out <- out[,1]
                             out
                           },
                           
                           bagEarth = 
                           {
                             library(earth)
                             if(modelFit$problemType == "Regression")
                               {
                                 out <- predict(modelFit, newdata)
                               } else {
                                 out <- as.character(predict(modelFit, newdata, type = "class"))
                               }
                             out
                           },
                           
                           
                           lm =, lmStepAIC =, ppr =, rlm =
                           {
                             library(MASS)
                             out <- predict(modelFit, newdata)
                             out
                           },
                           
                           gamboost =, blackboost =, glmboost =
                           {
                             library(mboost)
                             if(method == "glmboost") newdata <- as.matrix(newdata)
                             predType <- ifelse(modelFit$problemType == "Classification", "class", "response")
                             out <- predict(modelFit, newdata, type = predType)
                             if(modelFit$problemType == "Classification") out <- as.character(out)
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- as.vector(out)
                                 
                                 for(j in seq(along = param$.mstop))
                                   {
                                     tmp[[j+1]]  <- as.vector(
                                                              predict(modelFit[param$.mstop[j]],
                                                                      newdata,
                                                                      type = predType))
                                   }
                                 
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             
                             out
                           },
                           
                           ada =
                           {
                             library(ada)
                             out <- predict(modelFit, newdata)
                             out <-as.character(out)
                             out
                             
                           },
                           
                           ctree =
                           {
                             library(party)

                             out <- predict(modelFit, newdata)
                             if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                             if(is.matrix(out)) out <- out[,1]
                             out
                           },

                           ctree2 =
                           {
                             library(party)

                             out <- predict(modelFit, newdata)
                             if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                             if(is.matrix(out)) out <- out[,1]
                             out
                           },                           
                           
                           cforest =
                           {
                             library(party)
                             ## party builds the levels into the model object, so I'm
                             ## going to assume that all the levels will be passed to
                             ## the output
                             out <- predict(modelFit, newdata, OOB = TRUE)
                             if(is.matrix(out)) out <- out[,1]
                             if(!is.null(modelFit@responses@levels$.outcome)) out <-as.character(out)
                             
                             out
                           },
                           
                           lasso =, enet =
                           {
                             library(elasticnet)
                             out <- predict(modelFit, newdata, s = modelFit$tuneValue$.fraction, mode = "fraction")$fit

                             if(!is.null(param))
                               {
                                 if(nrow(param) > 1)
                                   {
                                     out <- c(
                                              list(if(is.matrix(out)) out[,1]  else out),
                                              as.list(
                                                      as.data.frame(
                                                                    predict(modelFit,
                                                                            newx = as.matrix(newdata),
                                                                            s = param$.fraction,
                                                                            mode = "fraction")$fit)))

                                   } else {
                                     tmp <- predict(modelFit,
                                                    newx = as.matrix(newdata),
                                                    s = param$.fraction,
                                                    mode = "fraction")$fit
                                     out <- c(list(if(is.matrix(out)) out[,1]  else out),  list(tmp))
                                   }
                               }
                             out
                           },

                           ridge =
                           {
                             library(elasticnet)
                             predict(modelFit, as.matrix(newdata), s = 1, mode = "fraction")$fit
                           },

                           sddaLDA =, sddaQDA =
                           {
                             library(SDDA)
                             predict(modelFit, as.matrix(newdata), type = "class")
                           },

                           logitBoost =
                           {
                             library(caTools)

                             out <- as.character(caTools::predict.LogitBoost(modelFit, newdata, type="class"))
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.nIter))
                                   {
                                     tmp[[j+1]] <- as.character(
                                                                caTools::predict.LogitBoost(
                                                                                            modelFit,
                                                                                            newdata,
                                                                                            nIter = param$.nIter[j]))
                                   }
                                 out <- tmp
                               }
                             out
                           },
                           M5Rules =, M5 =
                           {
                             library(RWeka)
                             predict(modelFit , newdata)
                           },
                           J48 =, LMT =, JRip =, OneR =, PART = 
                           {
                             library(RWeka)
                             out <- as.character(predict(modelFit , newdata))
                             out
                           },
                           superpc =
                           {
                             library(superpc)
                             
                             out <- superpc.predict(modelFit,
                                                    modelFit$data,
                                                    newdata = list(x=t(newdata)),
                                                    n.components = modelFit$tuneValue$.n.components,
                                                    threshold = modelFit$tuneValue$.threshold)$v.pred.1df
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.threshold))
                                   {
                                     tmp[[j+1]] <- superpc.predict(modelFit,
                                                                   modelFit$data,
                                                                   newdata = list(x=t(newdata)),
                                                                   threshold = param$.threshold[j],
                                                                   n.components = param$.n.components[j])$v.pred.1df
                                   }
                                 out <- tmp
                               }

                             out
                           },
                           penalized =
                           {
                             library(penalized)
                             if(attributes(modelFit)$model == "linear")
                               {
                                 out <- predict(modelFit, newdata)[, "mu"]
                               } else {
                                 out <- ifelse(predict(modelFit, newdata) > .5,
                                               modelFit$obsLevel[1],
                                               modelFit$obsLevel[2])
                               }
                             out
                           },
                           spls =
                           {
                             library(spls)
                             if(length(modelFit$obsLevels) < 2)
                               {
                                 predict(modelFit, newdata)
                               } else {
                                 as.character(caret:::predict.splsda(modelFit, newdata, type = "class"))
                               }
                           },
                           sda =
                           {
                             library(sda)
                             if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                             as.character(sda::predict.sda(modelFit, newdata)$class)
                           },
                           sparseLDA =
                           {
                             library(sparseLDA)
                             as.character(sparseLDA:::predict.sda(modelFit, newdata)$class)
                           },
                           smda =
                           {
                             library(sparseLDA)
                             as.character(predict(modelFit, newdata)$class)
                           },                           
                           glm =, glmStepAIC =, gam =, gamLoess =, gamSpline =
                           {
                             if(modelFit$problemType == "Classification")
                               {
                                 probs <-  predict(modelFit, newdata, type = "response")
                                 out <- ifelse(probs < .5,
                                               modelFit$obsLevel[1],
                                               modelFit$obsLevel[2])
                               } else {
                                 out <- predict(modelFit, newdata, type = "response")
                               }
                             out
                           },
                           mda =, pda =, pda2 = 
                           {
                             library(mda)
                             as.character(predict(modelFit, newdata))
                           },
                           glmnet =
                           {                          
                             library(glmnet)
                             if(!is.matrix(newdata)) newdata <- as.matrix(newdata)

                             if(!is.null(param))
                               {
                                 if(length(modelFit$obsLevels) < 2)
                                   {
                                     out <- as.list(as.data.frame(predict(modelFit, newdata, s = param$.lambda)))
                                   } else {
                                     out <- predict(modelFit, newdata, s = param$.lambda, type = "class")
                                     out <- as.list(as.data.frame(out, stringsAsFactors = FALSE))
                                   }
                               } else {
                                 
                                 if(is.null(modelFit$lambdaOpt))
                                   stop("optimal lambda not saved by train; needs a single lambda value")
                                 if(length(modelFit$obsLevels) < 2)
                                   {
                                     out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)[,1]
                                   } else {
                                     out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, type = "class")[,1]
                                   }
                               }
                             out
                           },
                           relaxo =
                           {
                             library(relaxo)
                             out <- predict(modelFit,
                                            as.matrix(newdata),
                                            lambda = modelFit$tuneValue$.lambda,
                                            phi = modelFit$tuneValue$.phi)
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.lambda))
                                   {
                                     tmp[[j+1]] <- predict(modelFit,
                                                           as.matrix(newdata),
                                                           lambda = param$.lambda[j],
                                                           phi = modelFit$tuneValue$.phi)
                                   }
                                 out <- tmp
                               }
                             
                             out
                           },
                           lars =
                           {
                             library(lars)
                             out <- predict(modelFit,
                                            as.matrix(newdata),
                                            type = "fit",
                                            mode = "fraction",
                                            s = modelFit$tuneValue$.fraction)$fit

                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.fraction))
                                   {
                                     tmp[[j+1]] <- predict(modelFit,
                                                           as.matrix(newdata),
                                                           type = "fit",
                                                           mode = "fraction",
                                                           s = param$.fraction[j])$fit
                                   }
                                 out <- tmp
                               }
                             out
                           },
                           lars2 =
                           {
                             library(lars)
                             out <- predict(modelFit,
                                            as.matrix(newdata),
                                            type = "fit",
                                            mode = "step",
                                            s = modelFit$tuneValue$.step)$fit

                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.step))
                                   {
                                     tmp[[j+1]] <- predict(modelFit,
                                                           as.matrix(newdata),
                                                           type = "fit",
                                                           mode = "step",
                                                           s = param$.step[j])$fit
                                   }
                                 out <- tmp
                               }
                             out
                           },
                           vbmpRadial =
                           {
                             library(vbmp)
                             probs <- predictCPP(modelFit, newdata)
                             out <- modelFit$obsLevels[apply(probs, 1, which.max)]
                             out
                           },
                           nodeHarvest =
                           {
                             library(nodeHarvest)
                             if(modelFit$problemType == "Regression")
                               {
                                 predict(modelFit, as.matrix(newdata), maxshow = 0)
                               } else  {
                                 prbs <- predict(modelFit, as.matrix(newdata), maxshow = 0)
                                 ifelse(prbs > .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                               }
                           },
                           Linda =, QdaCov =
                           {
                             library(rrcov)
                             predict(modelFit, newdata)@classification
                           },                           
                           stepLDA =, stepQDA =
                           {
                             library(MASS)
                             as.character(
                                          predict(modelFit$fit,
                                                  newdata[,  predictors(modelFit), drop = FALSE])$class)
                           },
                           plr =
                           {
                             library(stepPlr)
                             ifelse(predict(modelFit, as.matrix(newdata), type = "class") == 1,
                                    modelFit$obsLevels[1],
                                    modelFit$obsLevels[2])
                           },
                           GAMens =
                           {
                             library(GAMens)
                             predict(modelFit, newdata)$class[,1]
                           },
                           rocc =
                           {
                             library(rocc)
                             tmp <- p.rocc(modelFit, t(as.matrix(newdata)))
                             factor(ifelse(tmp == "1",  modelFit$obsLevels[1],  modelFit$obsLevels[2]),
                                    levels =  modelFit$obsLevels)
                           },
                           foba =
                           {
                             library(foba)
                             out <- predict(modelFit, newdata, k = modelFit$tuneValue$.k, type = "fit")$fit

                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.k))
                                   {
                                     tmp[[j+1]] <- predict(modelFit, newdata, k = param$.k[j], type = "fit")$fit
                                   }
                                 out <- tmp
                               }
                             out
                           },
                           partDSA =
                           {
                             library(partDSA)


                             if(!is.null(param))
                               {
                                 tmp <- c(modelFit$tuneValue$.cut.off.growth, param$.cut.off.growth)

                                 ## There are cases where the number of models saved by the function is
                                 ## less than the values in cut.off.growth (e.g. cut.off.growth = 1:10
                                 ## but partDSA only has 6 partitions). We will predict the "overage" using
                                 ## the largest model in the obejct (e.g. models 7:10 predicted by model 6).
                                 if(modelFit$problemType == "Classification")
                                   {
                                     out <- predict(modelFit, newdata)
                                     if(max(tmp) > ncol(out)) tmp[tmp > ncol(out)] <- ncol(out)
                                     out <- out[tmp]
                                     out <- lapply(out, as.character)
                                   } else {
                                     out <- predict(modelFit, newdata)
                                     if(max(tmp) > ncol(out)) tmp[tmp > ncol(out)] <- ncol(out)
                                     out <- out[,tmp, drop= FALSE]
                                     out <- as.list(as.data.frame(out))
                                   }
                               } else {

                                 ## use best Tune
                                 if(modelFit$problemType == "Classification")
                                   {
                                     out <- as.character(predict(modelFit, newdata)[[modelFit$cut.off.growth]])
                                   } else {
                                     out <- predict(modelFit, newdata)[,modelFit$cut.off.growth]
                                   }
                               }
                             out
                           },
                           hda =
                           {
                             library(hda)
                             tmp <- predict(modelFit, as.matrix(newdata))
                             if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                             as.character(predict(modelFit$naivebayes, tmp))
                           },
                           icr =
                           {
                             predict(modelFit, newdata)
                           },
                           neuralnet =
                           {
                             library(neuralnet)
                             newdata <- newdata[, modelFit$model.list$variables, drop = FALSE]
                             compute(modelFit,
                                     covariate = newdata)$net.result[,1]

                           },
                           qrf =
                           {
                             library(quantregForest)
                             out <- predict(modelFit, newdata, quantiles = .5)
                             if(is.matrix(out)) out <- out[,1]
                             out
                           },
                           bag =
                           {
                             predict(modelFit, newdata)
                           },
                           hdda =
                           {
                             library(HDclassif)
                             as.character(predict(modelFit, newdata)$class)
                           },
                           logreg =
                           {
                             library(LogicReg)
                             if(modelFit$type == "logistic")
                               {
                                 out <- ifelse(predict(modelFit, newbin = newdata) >= .5,
                                               modelFit$obsLevels[1], modelFit$obsLevels[2])
                               } else out <- predict(modelFit, newbin = newdata)
                             out
                           },
                           logforest =
                           {
                             library(LogicForest)
                             ifelse(predict(modelFit, newdata = newdata)$LFprediction == 1,
                                    modelFit$obsLevels[1], modelFit$obsLevels[2])
                           },
                           logicBag =
                           {
                             library(logicFS)
                             if(modelFit$problemType == "Classification")
                               {
                                 if(length(modelFit$obsLevels) == 2)
                                   {
                                     as.character(modelFit$obsLevels[predict(modelFit, newData = newdata) + 1])
                                   } else {
                                     as.character(predict(modelFit, newData = newdata))
                                   }
                               } else predict(modelFit, newData = newdata)
                           },
                                        #                           plsGlmBinomial =, plsGlmGaussian =, plsGlmGamma =, plsGlmPoisson =
                                        #                           {
                                        #                             library(plsRglm)
                                        #                             out <- predict(modelFit$FinalModel, newdata = newdata, type = "response")
                                        #                             ## glm models the second factor level. See Details in ?glm
                                        #                             if(modelFit$family$family == "binomial")
                                        #                               {
                                        #                                 out <- ifelse(out> .5, modelFit$obsLevel[2], modelFit$obsLevel[1])
                                        #                               }
                                        #                             out
                                        #                           },
                           qrnn =
                           {
                             library(qrnn)
                             qrnn.predict(as.matrix(newdata), modelFit)[,1]
                           },
                           cubist =
                           {
                             library(Cubist)
                             predict(modelFit, newdata)
                           },
                           bstTree =
                           {                             
                             library(bst)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- predict(modelFit, newdata, type = "class", mstop = modelFit$tuneValue$.mstop)
                                 out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                               } else {
                                 out <- predict(modelFit, newdata, type = "response", mstop = modelFit$tuneValue$.mstop)
                               }
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.mstop))
                                   {
                                     if(modelFit$problemType == "Classification")
                                       {
                                         bstPred <- predict(modelFit, newdata, type = "class", mstop = param$.mstop[j])
                                         tmp[[j+1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                       } else {
                                         tmp[[j+1]]  <- predict(modelFit, newdata, type = "response", mstop = param$.mstop[j])
                                       }
                                   }
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                           },
                           bstLs =, bstSm = 
                           {                             
                             library(bst)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- predict(modelFit, newdata, type = "class", mstop = modelFit$tuneValue$.mstop)
                                 out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                               } else {
                                 out <- predict(modelFit, newdata, type = "response", mstop = modelFit$tuneValue$.mstop)
                               }
                             
                             if(!is.null(param))
                               {
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.mstop))
                                   {
                                     if(modelFit$problemType == "Classification")
                                       {
                                         bstPred <- predict(modelFit, newdata, type = "class", mstop = param$.mstop[j])
                                         tmp[[j+1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                       } else {
                                         tmp[[j+1]]  <- predict(modelFit, newdata, type = "response", mstop = param$.mstop[j])
                                       }
                                   }
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                           },
                           leapForward =, leapBackward =, leapSeq =
                           {
                             library(leaps)
                             foo <- function(b, x) x[,names(b),drop = FALSE] %*% b

                             path <- 1:(modelFit$nvmax - 1)
                             betas <- coef(modelFit, id = 1:(modelFit$nvmax - 1))

                             newdata <- cbind(rep(1, nrow(newdata)), as.matrix(newdata))
                             colnames(newdata)[1] <- "(Intercept)"
                             
                             out <- foo(betas[[length(betas)]], newdata)[,1]

                             if(!is.null(param))
                               {
                                 varList <- varSeq(modelFit)
                                 pList <- unlist(lapply(varList, length))
                                 idx <- rev(path[path %in% param$.nvmax])
                                 
                                 preds <- lapply(betas[idx], foo, x= newdata)
                                 preds <- do.call("cbind", preds)
                                 
                                 out <- as.data.frame(cbind(out, preds))
                               }
                             
                             out
                           },
                           ORFridge =, ORFpls =, ORFsvm =, ORFlog =
                           {
                             library(obliqueRF)
                             as.character(predict(modelFit, newdata))                             
                           },
                           evtree =
                           {
                             library(evtree)
                             out <- predict(modelFit, newdata)
                             if(is.factor(out)) out <- as.character(out)
                             out
                           },
                           PenalizedLDA =
                           {
                             library(penalizedLDA)

                             out0 <- predict(modelFit, newdata)$ypred
                             out <- out0[,ncol(out0)]
                             out <- modelFit$obsLevels[out]
                             
                             if(!is.null(param))
                               {
                                 tmp <- out0[, param$.K,drop = FALSE]
                                 tmp <- apply(tmp, 2, function(x, l) l[x], l = modelFit$obsLevels)
                                 out <- as.data.frame(cbind(out, tmp), stringsAsFactors = FALSE)                                 
                               }
                             
                             out
                           },
                           rFerns =
                           {
                             library(rFerns)
                             as.character(predict(modelFit, newdata))
                           },
                           xyf =, bdk =
                           {
                             library(kohonen)
                             predict(modelFit, as.matrix(newdata))$prediction
                           },
                           mlp =, mlpWeightDecay =, rbf =, rbfDDA = 
                           {
                             library(RSNNS)
                             out <- predict(modelFit, newdata)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
                               } else out <- out[,1]
                             out
                           },
                           RRF =, RRFglobal =  
                           {
                             library(RRF)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <-  as.character(predict(modelFit, newdata))
                               } else {
                                 out <- predict(modelFit, newdata)
                               }
                             out
                           },
                           krlsRadial =, krlsPoly =
                           {
                             library(KRLS)
                             predict(modelFit, newdata)$fit[,1]
                           },
                           C5.0 =, C5.0Tree =, C5.0Rules =
                           {
                             library(C50)
                             out <- as.character(predict(modelFit, newdata))

                             if(!is.null(param))
                               {
                                 tmp <- out
                                 out <- vector(mode = "list", length = nrow(param) + 1)
                                 out[[1]] <- tmp
                                 
                                 for(j in seq(along = param$.trials))
                                   {                                    
                                     out[[j+1]] <- as.character(predict(modelFit, newdata, trial = param$.trials[j]))
                                   }
                               }
                             out
                           },
                           custom =
                           {
                             custom(object = modelFit, newdata = newdata)
                           }
                           )
  predictedValue
}


