predictionFunction <- function(method, modelFit, newdata, param = NULL)
{
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL
  
  predictedValue <- switch(method,
                           lda =, rda =, gpls =
                           {
                             switch(method,
                                    lda = library(MASS),
                                    rda = library(klaR),
                                    gpls = library(gpls))
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
                                        # to correspond to gbmClasses definition above
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
                                        # to correspond to gbmClasses definition above
                                       } else {
                                         tmp[,j]  <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                       }
                                   }
                                 out <- cbind(out, tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.n.trees, param$.n.trees)
                                 
                               }
                             out
                           },
                           
                           rf =
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
                           svmRadial =, svmPoly =,
                           rvmRadial =, rvmPoly =,
                           lssvmRadial =, lssvmPoly =,
                           gaussprRadial =, gaussprPoly =
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
                             out <- as.character(predict(modelFit, newdata, type="class"))
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
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 # translate maxdepth to Cp: interpolate points in-between
                                 
                                 cpValues <- depth2cp(modelFit$cptable, param$.maxdepth)
                                 
                                 for(j in seq(along = cpValues))
                                   {
                                     prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                                     if(modelFit$problemType == "Classification")
                                       {
                                         tmp[,j] <- as.character(predict(prunedFit, newdata, type="class"))
                                       } else {
                                         tmp[,j]  <- predict(prunedFit, newdata, type="vector")
                                       }
                                   }
                                 out <- cbind(out, tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.maxdepth, param$.maxdepth)
                                 
                               }
                             out
                           },
                           
                           lvq =
                           {
                             library(class)
                             out <- as.character(lvqtest(modelFit , newdata))
                             out
                           },

                           pls =,
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
                                        # if we are in this block, ncomp the prediction should be a vector
                                     out <- cbind(as.character(out), tmp)
                                   } else {
                                     tmp <- if(length(param$.ncomp) > 1) predict(modelFit, newdata, ncomp = param$.ncomp)[,1,]
                                     else data.frame(pred = predict(modelFit, newdata, ncomp = param$.ncomp))
                                     out <- cbind(out, tmp)
                                   }

                                 out <- as.data.frame(out)
                                 attr(out, "values") <- c(modelFit$tuneValue$.ncomp, param$.ncomp)
                                 
                               }
                             out
                           },
                           
                           plsTest =,
                           {
                             library(pls)
                             
                             out <- if(modelFit$problemType == "Classification")
                               {
                                 if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                                 predict(modelFit, newdata, type="class")
                               } else as.vector(predict(modelFit, newdata, ncomp = max(modelFit$ncomp)))

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
                             
                             predClass <- as.character(pamr.predict(modelFit, t(newdata),
                                                                    threshold = modelFit$tuneValue$.threshold))
                             out <- factor(predClass,  levels = modelFit$obsLevels)
                             
                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.threshold))
                                   {
                                     tmp[,j] <- as.character(
                                                             pamr.predict(
                                                                          modelFit,
                                                                          t(newdata),
                                                                          threshold = param$.threshold[j]))
                                   }
                                 tmp <- cbind(as.character(out), tmp)
                                 out <- as.data.frame(tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.threshold, param$.threshold)
                               }
                             out
                           },
                           
                           nb =
                           {
                             library(klaR)
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
                             out <- predict(modelFit, newdata)
                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.nprune))
                                   {
                                     prunedFit <- update(modelFit, nprune = param$.nprune[j])
                                     tmp[,j]  <- predict(prunedFit, newdata)
                                   }
                                 out <- cbind(out, tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.nprune, param$.nprune)
                               }
                             out
                           },
                           
                           earthTest =
                           {
                             library(earth)
                             out <- predict(modelFit, newdata)
                             
                             out
                           },
                           
                           bagEarth =
                           {
                             library(earth)
                             out <- predict(modelFit, newdata)
                             out
                           },
                           
                           
                           lm =
                           {
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
                                 attr(out, "values") <- c(mstop(modelFit), param$.mstop)
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
                                        # see note in tuneScheme about this two lines:
                                 minMinCrit <- min(param$.mincriterion)
                                 param <- param[param$.mincriterion > minMinCrit,, drop = FALSE]
                                 
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.mincriterion))
                                   {
                                     tmp[,j] <- predict(modelFit, newdata, mincriterion = param$.mincriterion[j])
                                     if(!is.null(modelFit@responses@levels$.outcome)) tmp[,j] <-as.character(tmp[,j])
                                   }
                                 out <- cbind(out, tmp)
                                 
                                 attr(out, "values") <- c(minMinCrit, param$.mincriterion)
                                 
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
                                        # party builds the levels into the model object, so I'm
                                        # going to assume that all the levels will be passed to
                                        # the output
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
                                 tmp <- predict(modelFit, newx = as.matrix(newdata), s = param$.fraction, mode = "fraction")$fit
                                 out <- cbind(out, tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.fraction, param$.fraction)
                                 
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

                             out <- predict(modelFit, newdata, type="class")
                             
                             if(!is.null(param))
                               {
                                 tmp <- data.frame(
                                                   matrix(NA, nrow = nrow(newdata), ncol = nrow(param)),
                                                   stringsAsFactors = FALSE)
                                 
                                 for(j in seq(along = param$.nIter))
                                   {
                                     tmp[,j] <- as.character(
                                                             predict(
                                                                     modelFit,
                                                                     newdata,
                                                                     nIter = param$.nIter[j]))
                                   }
                                 out <- cbind(out, tmp)
                                 attr(out, "values") <- c(modelFit$tuneValue$.nIter, param$.nIter)
                                 
                               }
                             out
                           }
                           )
  predictedValue
}


