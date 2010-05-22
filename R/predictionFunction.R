predictionFunction <- function(method, modelFit, newdata, param = NULL)
{
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL

  coerceChar <- function(x)  as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
  
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.n.trees))
                                   {
                                     if(modelFit$problemType == "Classification")
                                       {
                                         gbmProb <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                         tmp[,j] <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                                         ## to correspond to gbmClasses definition above
                                       } else {
                                         tmp[,j]  <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                       }
                                   }
                                 
                                 out <- cbind(out, tmp)
                                 if(modelFit$problemType == "Classification") out <- coerceChar(out)
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
                             depth2cp <- function(x, depth)
                               {
                                 out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
                                 out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
                                 out
                               }

                             if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, newdata, type="class"))
                               } else {
                                 out  <- predict(modelFit, newdata, type="vector")

                               }

                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param) + 1),
                                                   stringsAsFactors = FALSE)
                                 
                                 ## translate maxdepth to Cp: interpolate points in-between
                                 tmp[, 1] <- out
                                 cpValues <- depth2cp(modelFit$cptable, param$.maxdepth)
                                 
                                 for(j in seq(along = cpValues))
                                   {
                                     prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[,j + 1] <- as.character(predict(prunedFit, newdata, type="class"))
                                       } else {
                                         tmp[,j + 1]  <- predict(prunedFit, newdata, type="vector")
                                       }
                                   }
                                 if(modelFit$problemType == "Classification") tmp <- coerceChar(tmp)
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
                                 if(modelFit$problemType == "Classification")
                                   {
                                     tmp <- if(length(param$.ncomp) > 1) predict(modelFit, newdata, ncomp = param$.ncomp)
                                     else data.frame(pred = predict(modelFit, newdata, ncomp = param$.ncomp))
                                     tmp <- as.data.frame(lapply(tmp, as.character), stringsAsFactors = FALSE)
                                     ## if we are in this block, ncomp the prediction should be a vector
                                     out <- cbind(as.character(out), tmp)
                                   } else {
                                     tmp <- if(length(param$.ncomp) > 1) predict(modelFit, newdata, ncomp = param$.ncomp)[,1,]
                                     else data.frame(pred = predict(modelFit, newdata, ncomp = param$.ncomp))
                                     out <- cbind(out, tmp)
                                   }

                                 if(modelFit$problemType == "Classification") out <- coerceChar(out)
                                 out <- as.data.frame(out)
                               }
                             out
                           },
                           
                           PLS =,
                           {
                             library(pls)
                             if(modelFit$problemType == "Classification")
                               {
                                 out <- as.character(predict(modelFit, as.matrix(newdata),  ncomp = modelFit$tuneValue$.ncomp))
                               } else {
                                 out <- as.vector(predict(modelFit, as.matrix(newdata), ncomp = modelFit$tuneValue$.ncomp))
                               }
                             out
                           },
                           
                           pam =
                           {
                             library(pamr)
                             
                             out <- as.character(pamr.predict(modelFit, t(newdata),
                                                              threshold = modelFit$tuneValue$.threshold))
                             
                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param) + 1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- out
                                 for(j in seq(along = param$.threshold))
                                   {
                                     tmp[,j + 1] <- as.character(
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)+1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- if(is.matrix(out)) out[,1] else out
                                 for(j in seq(along = param$.nprune))
                                   {
                                     prunedFit <- update(modelFit, nprune = param$.nprune[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[,j+1]  <-  as.character(predict(prunedFit, newdata,  type = "class"))
                                       } else {
                                         tmp[,j+1]  <-  predict(prunedFit, newdata)[,1]
                                       }
                                   }
                                 out <- tmp
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
                             out <- predict(modelFit, as.matrix(newdata), type = "response")
                             if(modelFit$problemType == "Classification") out <- as.character(out)

                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.mstop))
                                   {
                                     tmp[,j]  <- predict(modelFit[param$.mstop[j]], as.matrix(newdata), type = "response")
                                     if(modelFit$problemType == "Classification") tmp[,j] <- as.character(tmp[,j])
                                   }
                                 out <- cbind(out, tmp)
                                 if(modelFit$problemType == "Classification") out <- coerceChar(out)
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

                             if(!is.null(param))
                               {
                                 ## see note in tuneScheme about this two lines:
                                 minMinCrit <- min(param$.mincriterion)
                                 param <- param[param$.mincriterion > minMinCrit,, drop = FALSE]
                                 
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param) + 1),
                                                   stringsAsFactors = FALSE)
                                 ## See the examples in ?ctree. For regression, we get a matix and classifiaction
                                 ## we get a factor vector
                                 tmp[,1] <- if(is.matrix(out)) out[,1]  else out
                                 for(j in seq(along = param$.mincriterion))
                                   {
                                     tmpPred <- predict(modelFit, newdata, mincriterion = param$.mincriterion[j])
                                     if(!is.null(modelFit@responses@levels$.outcome)) tmpPred <-as.character(tmpPred)
                                     tmp[,j + 1] <- if(is.matrix(tmpPred)) tmpPred[,1]  else tmpPred
                                     
                                   }
                                 
                                 if(!is.null(modelFit@responses@levels$.outcome)) tmp <-  coerceChar(tmp)
                                 out <- tmp
                               }
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
                             if(!is.null(modelFit@responses@levels$.outcome)) out <-as.character(out)
                             
                             out
                           },
                           
                           lasso =, enet =
                           {
                             library(elasticnet)
                             out <- predict(modelFit, newdata, s = modelFit$tuneValue$.fraction, mode = "fraction")$fit
                             
                             if(!is.null(param))
                               {
                                 ## if length(fraction) == 1 then this is a vector, otherwise a matrix
                                 out2 <- predict(modelFit, newx = as.matrix(newdata), s = param$.fraction, mode = "fraction")$fit
                                 if(is.vector(out2)) out2 <- as.matrix(out2)
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param) + 1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- out
                                 for(j in seq(along = param$.fraction)) tmp[,j + 1] <- out2[,j]
                                 out <- tmp
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

                             out <- caTools::predict.LogitBoost(modelFit, newdata, type="class")
                             
                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.nIter))
                                   {
                                     tmp[,j] <- as.character(
                                                             caTools::predict.LogitBoost(
                                                                                         modelFit,
                                                                                         newdata,
                                                                                         nIter = param$.nIter[j]))
                                   }
                                 out <- cbind(out, tmp)
                                 if(modelFit$problemType == "Classification") out <-  coerceChar(out)
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in 1:nrow(param))
                                   {
                                     tmp[,j] <-  superpc.predict(
                                                                 modelFit,
                                                                 modelFit$data,
                                                                 newdata = list(x=t(newdata)),
                                                                 threshold = param$.threshold[j],
                                                                 n.components = param$.n.components[j])$v.pred.1df
                                   }
                                 tmp <- cbind(out, tmp)
                                 out <- as.data.frame(tmp)
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
                           glm =, glmStepAIC =
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
                                     out <- as.data.frame(predict(modelFit, newdata, s = param$.lambda))
                                   } else {
                                     tmp <- predict(modelFit, newdata, s = param$.lambda, type = "class")
                                     if(length(modelFit$obsLevels) == 2)
                                       {
                                         tmp <- apply(tmp, 1, function(x, y) y[x], y = modelFit$obsLevels)
                                         out <- as.data.frame(t(tmp), stringsAsFactors = FALSE)
                                       } else {
                                         out <- as.data.frame(tmp, stringsAsFactors = FALSE)
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)+1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- out
                                 for(j in seq(along = param$.lambda))
                                   {
                                     tmp[,j+1]  <-  predict(modelFit,
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)+1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- out
                                 for(j in seq(along = param$.fraction))
                                   {
                                     tmp[,j+1]  <-  predict(modelFit,
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)+1),
                                                   stringsAsFactors = FALSE)
                                 tmp[,1] <- out
                                 for(j in seq(along = param$.step))
                                   {
                                     tmp[,j+1]  <-  predict(modelFit,
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
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.k))
                                   {
                                     tmp[,j] <- predict(modelFit, newdata, k = param$.k[j], type = "fit")$fit
                                   }
                                 out <- cbind(out, tmp)
                               }
                             out
                           },
                           partDSA =
                           {
                             library(partDSA)


                             if(!is.null(param))
                               {
                                 tmp <- c(modelFit$tuneValue$.cut.off.growth, param$.cut.off.growth)
                                 if(modelFit$problemType == "Classification")
                                   {
                                     out <- predict(modelFit, newdata)
                                     out <- out[tmp]
                                     out <- lapply(out, as.character)
                                     out <- as.data.frame(do.call("cbind", out), stringsAsFactors = FALSE)
                                     
                                   } else {
                                     out <- as.data.frame(predict(modelFit, newdata)[,tmp, drop= FALSE])
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
                           }
                           )
  predictedValue
}


