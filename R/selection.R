

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
           hda =
           {
             x[order(x$newdim, -x$lambda, x$gamma),] 
           },           
           gbm =
           {
             # This is a toss-up, but the # trees probably adds
             # complexity faster than number of splits
             x[order(x$n.trees, x$interaction.depth, x$shrinkage),] 
           },
           bstTree =
           {
             x[order(x$mstop, x$maxdepth, x$nu),] 
           },
           bstLs =, bstSm = 
           {
             x[order(x$mstop, x$nu),] 
           },            
           rf =, rfNWS =, rfLSF =, parRF =, gpls =, pcr =, pls =, simpls =, widekernelpls =, PLS =, pam =, cforest =,
           nb =, ctree2 =, logitBoost=, J48 =, LMT =, ppr =, mda =, pda =, pda2 =,
           lars =, lars2 =, Linda =, QdaCov =, icr =, qrf =,Boruta =,
           leapForward=, leapBackward=, leapSeq =, rpart2 =, ORFridge =, ORFpls =, ORFsvm =, ORFlog =, evtree =  
           {
             x[order(x[,1]),]
           },
           rpart =, JRip =
           {
             x[order(x[,1], decreasing = TRUE),]
           },
           M5Rules =
           {
             x$pruned <- factor(as.character(x$pruned), levels = c("Yes", "No"))
             x$smoothed <- factor(as.character(x$smoothed), levels = c("Yes", "No"))
             x[order(x$pruned, x$smoothed),]
           },
           M5 =
           {
             x$pruned <- factor(as.character(x$pruned), levels = c("Yes", "No"))
             x$smoothed <- factor(as.character(x$smoothed), levels = c("Yes", "No"))
             x$rules <- factor(as.character(x$rules), levels = c("Yes", "No"))
             x[order(x$pruned, x$smoothed, x$rules),]
           },             
           svmradial =, svmRadial =
           {
             # If the cost is high, the decision boundary will work hard to
             # adapt. Also, if C is fixed, smaller values of sigma yeild more
             # complex boundaries
             x[order(x$C, -x$sigma),]
           },
           svmRadialCost =
           {
             x[order(x$C),]
           },
           svmpoly =, svmPoly =
           {
             x[order(x$degree, x$C, x$scale),]
           },
           svmLinear =
           {
             x[order(x$C),]
           },           
           rvmRadial=, lssvmRadial =, gaussprRadial =
           {
             x[order(-x$sigma),]
           },
           rvmPoly =, lssvmPoly =, gaussprPoly =
           {
             x[order(x$degree, x$scale),]
           },           
           avNNet =, nnet =, pcaNNet =
           {
             x[order(x$size, -x$decay),]
           },
           rrlda =
           {
             x[order(-x$lambda),]
           },           
           qrnn =
           {
             x[order(x$n.hidden, -x$penalty),]
           },           
           knn =, multinom =
           {
             x[order(-x[,1]),]
           },
           lvq =
           {
             x[order(-x$k, -x$size),]
           },
           mars =, earth =, fda =, bagEarth =, bagFDA =
           {
             x[order(x$degree, x$nprune),]
           },
           gcvEarth =
           {
             x[order(x$degree),]
           },           
           treebag =, lda =, lm =, sddaLDA =, sddaQDA =,
           glmStepAIC =,lmStepAIC =, slda =, glm =, qda =, OneR =,
           rvmLinear =, lssvmLinear =, gaussprLinear =,
           rlm =, vbmpRadial =, glmrob =, logforest =
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
             x[order(x$iter, x$maxdepth, x$nu),]
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
           ridge =
           {
             x[order(-x$lambda),]
           },           
           lasso =
           {
             x[order(x$fraction),]

           },
           superpc =
           {
             x[order(x$threshold, x$n.components),]
           },
           sparseLDA =
           {
             x[order(x$NumVars, -x$lambda),]
           },
           relaxo =
           {
             x[order(x$phi, -x$lambda),]
           },           
           penalized =
           {
             x[order(x$lambda1, x$lambda2),]
           },
           spls =, splsda = 
           {
             x[order(-x$eta, x$K),]
           },
           smda =
           {
             x[order(x$NumVars, x$R, -x$lambda),]
           },
           nodeHarvest =
           {
             x[order(x$maxinter, x$mode),]
           },
           plr =
           {
             x[order(-x$lambda),]
           },
           partDSA =
           {
             x[order(x$cut.off.growth, x$MPD),]
           },
           bag =
           {
             x[order(x$vars),]
           },
           hdda =
           {
             x[order(-x$threshold),]
           },
           plsGlmBinomial =, plsGlmGaussian =, plsGlmGamma =, plsGlmPoisson =
           {
             x[order(x$nt),]
           },
           cubist =
           {
             x[order(x$committees,  x$neighbors),]
           },
           logreg = x[order(x$ntrees, x$treesize),],
           logicBag = x[order(x$ntrees, x$nleaves),],
           neuralnet = x[order(x$layer1, x$layer2, x$layer3),],
           scrda = x[order(x$alpha, x$delta, decreasing = TRUE),],
           rocc = x[order(x$xgenes),],
           foba = x[order(x$k, -x$lambda),], 
           obliqueTree = x[order(x$variable.selection),],
           PART = x[order(x$pruned, -x$threshold),],
           sda = x[order(x$diagonal),],
           glmnet = x[order(-x$lambda, x$alpha),],
           stepLDA =, stepQDA = x[order(x$maxvar),],
           GAMens = x[order(x$iter, x$rsm_size),],
           gam = x[order(x$select),],
           gamLoess =  x[order(x$degree, -x$span),],
           gamSpline =  x[order(x$df),],
           stop("no sorting routine for this model")
           )

  }

## In these functions, x is the data fram of performance values and tuning parameters.

best <- function(x, metric, maximize)
  {

    bestIter <- if(maximize) which.max(x[,metric])
    else which.min(x[,metric])   

    bestIter
  }

oneSE <- function(x, metric, num, maximize)
  {
    index <- 1:nrow(x)
    
    if(!maximize)
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

tolerance <- function(x, metric, tol = 1.5, maximize)
  {
       
    index <- 1:nrow(x)
    
    if(!maximize)
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


