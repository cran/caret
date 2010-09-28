predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
{
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL

  coerceChar <- function(x)  as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)


  if(!is.null(preProc)) newdata <- predict(preProc, newdata)
  
  predictedValue <- switch(method,
                           lda =, rda =, gpls =, slda =, qda =
                           {
                             switch(method,
                                    lda =, qda = library(MASS),
                                    rda        = library(klaR),
                                    gpls       = library(gpls),
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
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out
                                 
                                 for(j in seq(along = param$.n.trees))
                                   {
                                     if(modelFit$problemType == "Classification")
                                       {
                                         gbmProb <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                         tmp[[j+1]] <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                         ## to correspond to gbmClasses definition above
                                       } else {
                                         tmp[[j+1]]  <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                       }
                                   }
                                 out <- if(modelFit$problemType == "Classification") lapply(tmp, as.character) else tmp
                               }
                             out
                           },
                           
                           rf =, parRF =
                           {
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
                           gaussprRadial =, gaussprPoly =, gaussprLinear =
                           {
                             library(kernlab)
                             if(is.character(lev(modelFit)))
                               {
                                 predClass <- as.character(predict(modelFit, newdata))
                                 out <- factor(predClass, levels = lev(modelFit))
                               } else {
                                 out <- predict(modelFit, newdata)
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
                           
                           nnet =, multinom =, pcaNNet =
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
                           
                           lvq =
                           {
                             library(class)
                             out <- as.character(lvqtest(modelFit , newdata))
                             out
                           },

                           pcr=, pls =,
                           {
                             library(pls)
                             
                             out <- if(modelFit$problemType == "Classification")
                               {
                                 if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                                 out <- predict(modelFit, newdata, type="class")
                                 
                               } else as.vector(predict(modelFit, newdata, ncomp = max(modelFit$ncomp)))

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
                             out
                           },

                           ctree2 =
                           {
                             library(party)

                             out <- predict(modelFit, newdata)
                             if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
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

                           sddaLDA =, sddaQDA =
                           {
                             library(SDDA)
                             predict(modelFit, as.matrix(newdata), type = "class")
                           },

                           logitBoost =,
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
                           M5Rules =
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
                             if(attributes(modelFit, "model")$model == "linear")
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
                                     tmp <- predict(modelFit, newdata, s = param$.lambda, type = "class")
                                     if(length(modelFit$obsLevels) == 2)
                                       {
                                         tmp <- apply(tmp, 1, function(x, y) y[x], y = modelFit$obsLevels)
                                         out <- as.list(as.data.frame(t(tmp), stringsAsFactors = FALSE))
                                       } else {
                                         tmp <- predict(modelFit, newdata, s = param$.lambda, type = "class")
                                         ## When predicting one sample, it downclasses the results to a vector..
                                         if(nrow(newdata) == 1) tmp <- matrix(tmp, nrow = 1)
                                         out <- as.list(as.data.frame(tmp, stringsAsFactors = FALSE))
                                       }
                                   }
                               } else {
                                 
                                 if(is.null(modelFit$lambdaOpt))
                                   stop("optimal lambda not saved by train; needs a single lambda value")
                                 if(length(modelFit$obsLevels) < 2)
                                   {
                                     out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)[,1]
                                   } else {
                                     out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, type = "class")[,1]
                                     if(length(modelFit$obsLevels) == 2) out <- modelFit$obsLevels[out]
                                   }
                               }
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
                                     out <- as.character(predict(modelFit, newdata)[[modelFit$.cut.off.growth]])
                                   } else {
                                     out <- predict(modelFit, newdata)[,modelFit$.cut.off.growth]
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
                             predict(modelFit, newdata, quantiles = .5)
                           },
                           scrda =
                           {
                             library(rda)
                             out <- predict(modelFit,
                                            x = modelFit$data$x,
                                            y = as.numeric(modelFit$data$y),
                                            xnew = t(as.matrix(newdata)),
                                            alpha = modelFit$tuneValue$.alpha,
                                            delta = modelFit$tuneValue$.delta)
                             out <- as.character(modelFit$obsLevels)[out]

                             if(!is.null(param))
                               {
                                 ## We could get results for all alpha, delta and samples at the same time.
                                 ## If #alpha > 1 and #delta > 1, the results are a 3d array. However, if either
                                 ## alpha or delta have one value, the array dinesions drop, so it is hard to
                                 ## get predictions elegently.
                                 
                                 ## Column order will be (a_1, d_1), (a_1, d_2), ..., (a_p, d_q)
                                 tmp <- vector(mode = "list", length = nrow(param) + 1)
                                 tmp[[1]] <- out

                                 ## Using predict.rda, the alpha and delta params will generate all
                                 ## possible combinations, which might not be what the user wanted
                                 ## as specified by tuneGrid. To make sure these match, we will
                                 ## loop over one parameter
                               
                                 uniqueA <- unique(param$.alpha)
                                 index1 <- 2
                                 for(i in 1:length(uniqueA))
                                   {
                                     delta <- subset(param, .alpha == uniqueA[i])$.delta
                                     index2 <- index1 + length(delta) - 1
                                     tmpPred <- predict(modelFit,
                                                        x = modelFit$data$x,
                                                        y = as.numeric(modelFit$data$y),
                                                        xnew = t(as.matrix(newdata)),
                                                        alpha = uniqueA[i],
                                                        delta = delta)
                                     ## If length(uniqueA) == 1 and length(delta) == 1, tmpPred
                                     ## gets downcast into a vector (ordinarily a matrix)
                                     if(is.vector(tmpPred)) tmpPred <- matrix(tmpPred, nrow = 1)
                                     tmpPred <- apply(tmpPred, 2, function(x, y) y[x], y = as.character(modelFit$obsLevels))
                                      if(is.vector(tmpPred)) tmpPred <- matrix(tmpPred, nrow = 1)
                                     tmp[index1:index2] <- as.list(as.data.frame(t(tmpPred)))
                                     index1 <- index2 + 1
                                   }
                                 out <- lapply(tmp, as.character) 
                               }

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
                           }
                           )
  predictedValue
}


