probFunction <- function(method, modelFit, newdata)
{
  
  if(!any(modelLookup(method)$probModel))
    stop("no probability method for this model")
  
  
  if(method %in% c(
                   "svmradial", "svmpoly",
                   "svmRadial", "svmPoly",
                   "gaussprRadial", "gaussprPoly",
                   "lssvmRadial",
                   "ctree", "ctree2",  "cforest"))
    {
      
      obsLevels <- switch(method,
                          svmradial =, svmpoly =,
                          svmRadial =, svmPoly =,
                          gaussprRadial =, gaussprPoly =,
                          lssvmRadial =
                          {
                            library(kernlab)
                            lev(modelFit)
                          },
                          
                          ctree =, cforest =
                          {
                            library(party)
                            levels(modelFit@data@get("response")[,1])
                          })
    } else {
      obsLevels <- modelFit$obsLevels
    }
  
  if(any(colnames(newdata) == ".outcome")) newdata$.outcome <- NULL
  
  classProb <- switch(method,
                      lda =, rda =, slda =
                      {
                        switch(method,
                               lda =  library(MASS),
                               rda =  library(klaR),
                               slda = library(ipred),
                               sda  = library(sparseLDA))
                        
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
                        out
                      },
                      rf =, rpart =, treebag  =
                      {
                        library(randomForest)
                        out <- predict(modelFit, newdata, type = "prob")
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
                        out <-pamr.predict(modelFit, t(newdata),
                                           threshold = modelFit$tuneValue$.threshold, type= "posterior")
                        out
                      },
                      nb =
                      {
                        library(klaR)
                        out <- predict(modelFit, newdata, type = "raw")$posterior
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
                        # glmboost defies conveintion a bit by having higher values of the lp
                        # correspond to the second factor level (as opposed to the first),
                        # so we use the -lp for the first factor level prob
                        library(mboost)
                        lp <- predict(modelFit, as.matrix(newdata), type = "lp")
                        out <- cbind(
                                     binomial()$linkinv(-lp),
                                     1 - binomial()$linkinv(-lp))
                        colnames(out) <- modelFit$obsLevels
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
                        out <- caTools::predict(modelFit, newdata, type = "raw")
                        # I've seen them not be on [0, 1]
                        out <- t(apply(out, 1, function(x) x/sum(x)))
                        out
                      },
                      J48 =, LMT =, JRip =
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
                      }
                      )

  if(!is.data.frame(classProb)) classProb <- as.data.frame(classProb)
  classProb[, obsLevels]
}

