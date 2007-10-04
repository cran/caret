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
         predClass <- as.character(predict(modelFit, newdata)$class)
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },
      
      gbm = 
      {
         library(gbm)
         if(is.null(param))
         {
            if(modelFit$problemType == "Classification")
            {      
               gbmProb <- predict(modelFit, newdata, type = "response", 
                  n.trees = modelFit$tuneValue$.n.trees)
               gbmClass <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2]) # to correspond to gbmClasses definition above
               out <- factor(gbmClass, levels = modelFit$obsLevels)          
            } else {
               out <- predict(modelFit, newdata, type = "response", 
                  n.trees = modelFit$tuneValue$.n.trees)
            }
         } else {
         
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
                       
            for(j in seq(along = param$.n.trees))
            {
               if(modelFit$problemType == "Classification")
               {      
                  gbmProb <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j])
                  gbmClass <- ifelse(gbmProb >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2]) # to correspond to gbmClasses definition above
                  out[,j] <- factor(gbmClass, levels = modelFit$obsLevels)
                  
               } else {
                  out[,j]  <- predict(modelFit, newdata, type = "response", n.trees = param$.n.trees[j]) 
               }
            }
         
         }      
         out
      },
      
      rf = 
      {
         if(modelFit$problemType == "Classification")
         {
            predClass <-  as.character(predict(modelFit, newdata))
            out <- factor(predClass, levels = modelFit$obsLevels)
         } else {
            out <- predict(modelFit, newdata)
         }
         out
      },
      
      svmradial =, svmpoly =
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
         predClass <- as.character(predict(modelFit, newdata, type="class"))
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },
   
      nnet =, multinom = 
      {
         library(nnet)
         if(modelFit$problemType == "Classification")
         {       
            predClass <- as.character(predict(modelFit, newdata, type="class"))
            out <- factor(predClass, levels = modelFit$obsLevels)
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

         if(is.null(param))
         {
            if(modelFit$problemType == "Classification")
            {      
               predClass <- as.character(predict(modelFit, newdata, type="class"))
               out  <- factor(predClass, levels = modelFit$obsLevels)
            } else {
               out  <- predict(modelFit, newdata, type="vector")      
   
            }
         } else {
         
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
            
            # translate maxdepth to Cp: interpolate points in-between
            
            cpValues <- depth2cp(modelFit$cptable, param$.maxdepth)
            
            for(j in seq(along = cpValues))
            {
               prunedFit <- prune.rpart(modelFit, cp = cpValues[j])
               if(modelFit$problemType == "Classification")
               {      
                  predClass <- as.character(predict(prunedFit, newdata, type="class"))
                  out[,j]  <- factor(predClass, levels = modelFit$obsLevels)
               } else {
                  out[,j]  <- predict(prunedFit, newdata, type="vector")      
               }
            }
         
         }
         out            
      },           
      
      lvq = 
      {
         library(class)
         predClass <- as.character(lvqtest(modelFit , newdata))
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },

      pls =, 
      {             
         library(pls)
        
         if(is.null(param))
         {
            out <- if(modelFit$problemType == "Classification")
            {      
               if(!is.matrix(newdata)) newdata <- as.matrix(newdata)          
               predict(modelFit, newdata, type="class")
            } else as.vector(predict(modelFit, newdata, ncomp = max(modelFit$ncomp)))         

         } else {
            out <- predict(modelFit, newdata, ncomp = param$.ncomp)            
            
            if(modelFit$problemType == "Classification")
            {
               out <- lapply(out, function(u, y) factor(as.character(u), levels = y), y = modelFit$obsLevels)  
            }
            out <- as.data.frame(out)         

         }
         out
      },      
      
      PLS =, 
      {     
         library(pls)
         if(modelFit$problemType == "Classification")
         {      
            predClass <- as.character(predict(modelFit, as.matrix(newdata),  ncomp = modelFit$tuneValue$.ncomp))
            out <- factor(predClass, levels = modelFit$obsLevels) 
         } else {
            out <- as.vector(predict(modelFit, as.matrix(newdata), ncomp = modelFit$tuneValue$.ncomp))
         }
         out
      },
      
      pam =  
      {
         library(pamr)
         
         if(is.null(param))
         {
            predClass <- as.character(pamr.predict(modelFit, t(newdata), 
               threshold = modelFit$tuneValue$.threshold))
            out <- factor(predClass,  levels = modelFit$obsLevels)
         } else {
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
            
            for(j in seq(along = param$.threshold))
            {
               out[,j] <- as.character(
                  pamr.predict(
                     modelFit, 
                     t(newdata), 
                     threshold = param$.threshold[j]))
            }
            out <- lapply(out, function(u, y) factor(as.character(u), levels = y), y = modelFit$obsLevels)
            out <- as.data.frame(out)
         }
         out  
      },
      
      nb =  
      {
         library(klaR)
         predClass <- as.character(predict(modelFit , newdata)$class)
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },
            
      fda = 
      {
         library(mda)
         library(earth)         
         predClass <- as.character(predict(modelFit , newdata))
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },      
          
      bagFDA = 
      {
         library(mda)
         library(earth)
         predClass <- as.character(predict(modelFit , newdata))
         out <- factor(predClass, levels = modelFit$obsLevels)
         out
      },
                  
      treebag = 
      {
         library(ipred)
         if(modelFit$problemType == "Classification")
         {      
            bagClass <- as.character(predict(modelFit, newdata,  type = "class"))
            out <- factor(bagClass, levels = modelFit$obsLevels) 
         } else {
            out <- predict(modelFit, newdata)
         }         
         out
      },

      mars =
      {
         library(earth)
         out <- predict(modelFit, newdata)
         out
      },
      
      earth =
      {
         library(earth)
         if(is.null(param))
         {
            out <- predict(modelFit, newdata)
         } else {
         
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
                      
            for(j in seq(along = param$.nk))
            {
               prunedFit <- update(modelFit, nprune = param$.nk[j])
               out[,j]  <- predict(prunedFit, newdata)      
            }
         }         
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
         if(is.null(param))
         {
            out <- predict(modelFit, as.matrix(newdata), type = "response")
            if(modelFit$problemType == "Classification") out <- factor(out, levels = modelFit$obsLevels)  
         } else {
         
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
                      
            for(j in seq(along = param$.mstop))
            {
               out[,j]  <- predict(modelFit[param$.mstop[j]], as.matrix(newdata))  
               if(modelFit$problemType == "Classification") out[,j] <- factor(as.character(out[,j]), levels = modelFit$obsLevels)  
            }
         }  
         out
      },
      
      ada = 
      {
         library(ada)
         out <- predict(modelFit, newdata)
         out <- factor(out, levels = modelFit$obsLevels)          
         out      
      
      },
      
      ctree = 
      {
         library(party)

         if(is.null(param))
         {
            out <- predict(modelFit, newdata)
         } else {
         
            out <- data.frame(
               matrix(NA, nrow = nrow(newdata), ncol = nrow(param)), 
               stringsAsFactors = FALSE)
            
            for(j in seq(along = param$.mincriterion))
            {
               out[,j] <- predict(modelFit, newdata, mincriterion = param$.mincriterion[j])
            }
         
         }
       
         out
      },
      
      cforest = 
      {
         library(party)
         # party builds the levels into the model object, so I'm 
         # going to assume that all the levels will be passed to
         # the output         
         out <- predict(modelFit, newdata, OOB = TRUE)
       
         out
       }
      
#      lasso =, enet = 
#      {
#         library(elasticnet)
#         
#         if(is.null(param))         
#         {
#            out <- predict(modelFit, newdata, s = modelFit$tuneValue$.fraction, mode = "fraction", type = "fit")
#         } else {
#            out <- predict(modelFit, newdata, s = param$.fraction, mode = "fraction")$fit
#         }
#      
#         out         
#        
            
   )
   predictedValue
}

