# I need to add to trainControl and train in all packages

# This function sorts the tuning parameter matrix from
# least complex models to most complex models

byComplexity <- function(x, model)
  {
    switch(model,
           rda =
           {
             # since lds is less complex than qda, we
             # sort on lambda (larger are least complex)
             x[order(-x$lambda, x$gamma),] 
           },
           gbm =
           {
             # This is a toss-up, but the # trees probably adds
             # complexity faster than number of splits
             x[order(x$n.trees, x$interaction.depth, x$shrinkage),] 
           },
           rf =, rfNWS =, rfLSF =, gpls =, pls =, PLS =, pam =, cforest =,
           nb =, rpart =, ctree2 =, logitBoost=
           {
             x[order(x[,1]),]
           },
           svmradial =, svmRadial =
           {
             # If the cost is high, the decision boundary will work hard to
             # adapt. Also, if C is fixed, smaller values of sigma yeild more
             # complex boundaries
             x[order(x$C, -x$sigma),]
           },
           svmpoly =, svmPoly =
           {
             x[order(x$degree, x$C, x$scale),]
           },
           rvmRadial=, lssvmRadial =, gaussprRadial =
           {
  
             x[order(-x$sigma),]
           },
           rvmPoly =, lssvmPoly =, gaussprPoly =
           {
             x[order(x$degree, x$scale),]
           },           
           nnet =, pcaNNet =
           {
             x[order(x$size, -x$decay),]
           },
           knn =, lvq =, multinom =
           {
             x[order(-x[,1]),]
           },
           mars =, earth =, fda =, bagEarth =, bagFDA =
           {
             x[order(x$degree, x$nprune),]
           },
           treebag =, lda =, lm =, sddaLDA =, sddaQDA =
           {
             x
           },
           glmboost =, gamboost =
           {
             x[order(x$mstop, x$prune),]
           },
           blackboost =
           {
             x[order(x$mstop, x$maxdepth),]
           },
           ada =
           {
             x[order(x$iter, x$maxdepth, x$.nu),]
           },
           ctree =
           {
             # If this is the threshold for splitting, then
             # smaller thresholds means more complexity
             x[order(-x$mincriterion),]
           },
           enet =
           {
             x[order(x$fraction, -x$lambda),]
           },
           lasso =
           {
             x[order(x$fraction),]

           })

  }


best <- function(x, metric)
  {

    bestIter <- if(metric != "RMSE") which.max(x[,metric])
    else which.min(x[,metric])   

    bestIter
  }

oneSE <- function(x, metric, num)
  {
    index <- 1:nrow(x)
    
    if(metric == "RMSE")
      {
        bestIndex <- which.min(x[,metric])  
        perf <- x[bestIndex,metric] + (x[bestIndex,paste(metric, "SD", sep = "")])/sqrt(num)
        candidates <- index[x[, metric] <= perf]
        bestIter <- min(candidates)
      } else {
        bestIndex <- which.max(x[,metric])  
        perf <- x[bestIndex,metric] - (x[bestIndex,paste(metric, "SD", sep = "")])/sqrt(num)

        candidates <- index[x[, metric] >= perf]
        bestIter <- min(candidates)
      }
    bestIter
  }

tolerance <- function(x, metric, tol = 1.5)
  {
       
    index <- 1:nrow(x)
    
    if(metric == "RMSE")
      {
        best <- min(x[,metric])  
        perf <- (x[,metric] - best)/best * 100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
      } else {
        best <- max(x[,metric])  
        perf <- (x[,metric] - best)/best * -100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
      }
    bestIter
  }


