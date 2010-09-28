probFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
{
  
  if(!any(modelLookup(method)$probModel))
    stop("no probability method for this model")
  
  
  if(method %in% c(
                   "svmradial", "svmpoly",
                   "svmRadial", "svmPoly", "svmLinear",
                   "gaussprRadial", "gaussprPoly", "gaussprLinear",
                   "lssvmRadial", "lssvmLinear",
                   "ctree", "ctree2",  "cforest",
                   "penalized", "Linda", "QdaCov"))
    {
      
      obsLevels <- switch(method,
                          svmradial =, svmpoly =, 
                          svmRadial =, svmPoly =, svmLinear =,
                          gaussprRadial =, gaussprPoly =, gaussprLinear =,
                          lssvmRadial =, lssvmLinear =
                          {
                            library(kernlab)
                            lev(modelFit)
                          },

                          Linda =, QdaCov = names(modelFit@prior),
                          
                          ctree =, cforest =
                          {
                            library(party)
                            levels(modelFit@data@get("response")[,1])
                          })
    } else {
      obsLevels <- modelFit$obsLevels
    }
  
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL

  if(!is.null(preProc)) newdata <- predict(preProc, newdata)

  
  classProb <- switch(method,
                      lda =, rda =, slda =, qda =
                      {
                        switch(method,
                               lda =, qda =  library(MASS),
                               rda        =  library(klaR),
                               slda       = library(ipred),
                               sparseLDA  = library(sparseLDA))
                        
                        out <- predict(modelFit, newdata)$posterior
                        out
                      },

                      knn =
                      {
                        out <- predict(modelFit, newdata, type = "prob")
                        out
                        
                      },
                      
                      svmradial =, svmpoly =,
                      svmRadial =, svmPoly =,
                      lssvmRadial =,
                      gaussprRadial =, gaussprPoly =
                      {
                        library(kernlab)
                        
                        out <- predict(modelFit, newdata, type="probabilities")
                        out <- out[, lev(modelFit), drop = FALSE]
                        out
                      },
                      
                      gbm =
                      {
                        library(gbm)
                        out <- predict(modelFit, newdata, type = "response",
                                       n.trees = modelFit$tuneValue$.n.trees)
                        out <- cbind(out, 1-out)
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            
                            for(j in seq(along = param$.n.trees))
                              {
                                if(modelFit$problemType == "Classification")
                                  {
                                    gbmProb <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                                    gbmProb <- cbind(gbmProb, 1-gbmProb)
                                    dimnames(gbmProb)[[2]] <-  modelFit$obsLevels
                                    tmp[[j+1]] <- as.data.frame(gbmProb)
                                  }
                              }
                            out <- tmp
                          }
                        out
                      },

                      nnet =, pcaNNet =
                      {
                        library(nnet)
                        out <- predict(modelFit, newdata)
                        if(dim(as.data.frame(out))[2] == 1)
                          {
                            out <- cbind(out, 1-out)
                            dimnames(out)[[2]] <-  rev(modelFit$obsLevels)
                          }
                        out
                      },
                      pls =
                      {
                        library(pls)
                        if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                        out <- predict(modelFit, newdata, type = "prob",  ncomp = modelFit$tuneValue$.ncomp)
                        if(length(dim(out)) == 3) out <- out[,,1]
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            
                            for(j in seq(along = param$.ncomp))
                              {
                                tmpProb <- predict(modelFit, newdata, type = "prob",  ncomp = param$.ncomp[j])[,,1]
                                tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])
                              }
                            out <- tmp
                          }                        
                        out
                      },
                      rf =, treebag  =, parRF =
                      {
                        library(randomForest)
                        out <- predict(modelFit, newdata, type = "prob")            
                        out
                      },
                      rpart =
                      {
                        library(randomForest)
                        if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                        out <- predict(modelFit, newdata, type = "prob")
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            cpValues <- depth2cp(modelFit$cptable, param$.maxdepth)
                            
                            for(j in seq(along = cpValues))
                              {
                                prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
                                tmpProb <- predict(prunedFit, newdata, type = "prob")
                                tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])
                              }
                            out <- tmp
                          }                            
                        out
                      },                      
                      gpls =
                      {
                        library(gpls)
                        out <- predict(modelFit, newdata)$predicted
                        out <- cbind(out, 1-out)
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        out
                      },
                      pam =
                      {
                        library(pamr)
                        out <- pamr.predict(modelFit, t(newdata),
                                            threshold = modelFit$tuneValue$.threshold, type= "posterior")
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            
                            for(j in seq(along = param$.threshold))
                              {
                                tmpProb <-  pamr.predict(modelFit, t(newdata),
                                                         threshold =  param$.threshold[j], type= "posterior")
                                tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])

                              }
                            out <- tmp
                          }   
                        out
                      },
                      nb =
                      {
                        library(klaR)
                        out <- predict(modelFit, newdata, type = "raw")$posterior
                        out
                      },
 
                      earth =, bagEarth =
                      {
                        library(mda)
                        library(earth)
                        out <- predict(modelFit, newdata, type= "prob")                        
                        out
                      },

                      
                      fda =
                      {
                        library(mda)
                        library(earth)
                        out <- predict(modelFit, newdata, type= "posterior")
                        out
                      },
                      
                      bagFDA =
                      {
                        library(mda)
                        library(earth)
                        out <- predict(modelFit, newdata, type= "probs")
                        out
                      },
                      
                      multinom =
                      {
                        library(nnet)
                        out <- predict(modelFit, newdata, type = "probs")
                        if(dim(as.data.frame(out))[2] == 1)
                          {
                            out <- cbind(out, 1-out)
                            dimnames(out)[[2]] <-  rev(modelFit$obsLevels)
                          }
                        out
                      },
                      ctree =, ctree2=, cforest =
                      {
                        library(party)
                        rawProbs <- treeresponse(modelFit, newdata)
                        probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                        out <- data.frame(probMatrix)
                        colnames(out) <- obsLevels
                        rownames(out) <- NULL
                        out
                      },
                      gamboost =, blackboost =, glmboost =
                      {
                        ## glmboost defies conveintion a bit by having higher values of the lp
                        ## correspond to the second factor level (as opposed to the first),
                        ## so we use the -lp for the first factor level prob
                        library(mboost)
                        if(method == "glmboost" & !is.matrix(newdata)) newdata <- as.matrix(newdata)
                        
                        lp <- predict(modelFit, newdata)
                        out <- cbind(
                                     binomial()$linkinv(-lp),
                                     1 - binomial()$linkinv(-lp))
                        colnames(out) <- modelFit$obsLevels
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            
                            for(j in seq(along = param$.mstop))
                              {                           
                                tmpProb <- predict(modelFit[param$.mstop[j]], newdata)
                                tmpProb <- cbind(binomial()$linkinv(-tmpProb),
                                                 1 - binomial()$linkinv(-tmpProb))
                                colnames(tmpProb) <- modelFit$obsLevels
                                tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])           
                              }
                            out <- tmp
                          }                        
                        out
                      },
                      ada =
                      {
                        library(ada)
                        out <- predict(modelFit, newdata, type = "prob")
                        colnames(out) <-  modelFit$obsLevels
                        out
                      },
                      sddaLDA =, sddaQDA =
                      {
                        library(SDDA)
                        predict(modelFit, as.matrix(newdata), type = "prob")
                      },
                      logitBoost =
                      {
                        library(caTools)
                        out <- caTools::predict.LogitBoost(modelFit, newdata, type = "raw")
                        ## I've seen them not be on [0, 1]
                        out <- t(apply(out, 1, function(x) x/sum(x)))
                        if(!is.null(param))
                          {
                            tmp <- vector(mode = "list", length = nrow(param) + 1)
                            tmp[[1]] <- out
                            
                            for(j in seq(along = param$.nIter))
                              {                           
                                tmpProb <- caTools::predict.LogitBoost(modelFit,
                                                                       newdata,
                                                                       type = "raw",
                                                                       nIter = param$.nIter[j])
                                tmpProb <- out <- t(apply(tmpProb, 1, function(x) x/sum(x)))
                                tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels])           
                              }
                            out <- tmp
                          }                       
                        out
                      },
                      J48 =, LMT =, JRip =, OneR =, PART = 
                      {
                        library(RWeka)
                        out <- predict(modelFit,
                                       newdata,
                                       type = "probability")
                        out
                      },
                      penalized =
                      {
                        library(penalized)
                        out <- predict(modelFit, newdata)
                        out <- cbind(out, 1-out)
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        out
                      },
                      spls =
                      {
                        library(spls)
                        if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                        caret:::predict.splsda(modelFit, newdata, type = "prob")
                      },
                      sda =
                      {                  
                        library(sda)
                        if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                        sda::predict.sda(modelFit, newdata)$prob
                      },
                      glm =, gam =, gamLoess =, gamSpline =
                      {
                        
                        out <- predict(modelFit, newdata, type = "response")
                        out <- cbind(1-out, out)
                        ## glm models the second factor level. See Details in ?glm
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        out
                      },
                      mda =, pda =, pda2 =
                      {
                        library(mda)
                        predict(modelFit, newdata, type = "posterior")
                      },
                      vbmpRadial =
                      {
                        library(vbmp)
                        probs <- predictCPP(modelFit, newdata)
                        colnames(probs) <- obsLevels
                        probs
                      },
                      glmnet =
                      {
                        
                        if(!is.null(param))
                          {
                            probs <- predict(modelFit,
                                             as.matrix(newdata),
                                             s = param$.lambda,
                                             type = "response")

                            probs <- as.list(as.data.frame(probs))
                            probs <- lapply(probs,
                                            function(x, lev)
                                            {
                                              tmp <- data.frame(x, 1-x)
                                              names(tmp) <- lev
                                              tmp
                                            },
                                            lev = modelFit$obsLevels)
                            
                          } else {
                            probs <- predict(modelFit,
                                             as.matrix(newdata),
                                             s = modelFit$lambdaOpt,
                                             type = "response")
                            probs <- cbind(1-probs, probs)
                            colnames(probs) <- modelFit$obsLevels
                          }
                        
                        probs
                      },
                      nodeHarvest =
                      {
                        predict(modelFit, as.matrix(newdata), maxshow = 0)
                      },
                      Linda =, QdaCov =
                      {
                        library(rrcov)
                        probs <- predict(modelFit, newdata)@posterior
                        colnames(probs) <- names(modelFit@prior)
                        probs
                      },
                      stepLDA =, stepQDA =
                      {
                        library(MASS)
                        predict(modelFit$fit,
                                newdata[, predictors(modelFit), drop = FALSE])$posterior
                      },
                      plr =
                      {
                        library(stepPlr)
                        out <- predict(modelFit, newdata, type = "response")
                        out <- cbind(out, 1-out)
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        out
                      },
                      GAMens =
                      {
                        library(GAMens)
                        out <- predict(modelFit, newdata)$pred
                        out <- cbind(1 - out, out)
                        dimnames(out)[[2]] <-  modelFit$obsLevels
                        out

                      },
                      hda =
                      {
                        library(hda)
                        tmp <- predict(modelFit, as.matrix(newdata))
                        if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                        predict(modelFit$naivebayes, tmp, type = "raw")
                      },
                      scrda =
                      {
                        library(rda)
                        tmp <- predict(modelFit,
                                       x = modelFit$data$x,
                                       y = as.numeric(modelFit$data$y),
                                       xnew = t(as.matrix(newdata)),
                                       type = "posterior",
                                       alpha = modelFit$tuneValue$.alpha,
                                       delta = modelFit$tuneValue$.delta)
                        ## No matter the actual factor levels supplied, we
                        ## need to back-transform the column names
                        colnames(tmp) <- obsLevels[as.numeric(colnames(tmp))]
                        tmp
                      },
                      bag =
                      {
                        predict(modelFit, newdata, type = "prob")

                      },
                      hdda =
                      {
                        library(HDclassif)
                        predict(modelFit, newdata)$posterior
                      },
                      logreg =
                      {
                        library(LogicReg)
                        tmp <- predict(modelFit, newbin = newdata)
                        out <- cbind(tmp, 1 - tmp)
                        names(out) <- modelFit$obsLevels
                        out
                      },
                      logforest =
                      {
                        library(LogicForest)
                        tmp <- predict(modelFit, newdata = newdata)$proportion_one
                        out <- as.data.frame(cbind(tmp, 1 - tmp))
                        names(out) <- modelFit$obsLevels
                        out
                      },
                      logicBag =
                      {
                        library(logicFS)

                        if(length(modelFit$obsLevels) == 2)
                          {
                            out <- predict(modelFit, newData = newdata, type = "prob")
                            out <- as.data.frame(cbind(out, 1 - out))
                            colnames(out) <- modelFit$obsLevels
                          } else {
                            out <- predict(modelFit, newData = newdata, type = "prob")
                          }
                        out
                      }
                      )

  if(!is.data.frame(classProb) & is.null(param))
    {
      classProb <- as.data.frame(classProb)
      classProb <- classProb[, obsLevels]
    }
  classProb
}

