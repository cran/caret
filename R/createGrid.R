"createGrid" <-
  function(method, len = 3, data = NULL, pp = NULL)
{

  somDims <- function(x)
    {
      out <- expand.grid(.xdim = 1:x, .ydim = 2:(x+1),
                         .xweight = seq(.5, .9, length = len))
      out$.topo <- "hexagonal"
      subset(out, .xdim>= .ydim)
    }
  ## rpart needs its own function since we fit an initial model,
  ## read off the possible complexity parameters and use
  ## those values to devleop the grid. 
  rpart2Tune <- function(data, len)
    {
      library(rpart)
      initialFit <- rpart(
                          .outcome ~ .,
                          data,
                          control = rpart.control(cp = 0))$cptable
      initialFit <- initialFit[order(-initialFit[,"CP"]), "nsplit", drop = FALSE]
      initialFit <- initialFit[initialFit[,"nsplit"] > 0 & initialFit[,"nsplit"] <= 30, , drop = FALSE]
      if(dim(initialFit)[1] < len)
        {
          cat(
              "note: only",
              dim(initialFit)[1],
              "possible values of the max tree depth from the initial fit.\n",
              "Truncating the grid to",
              dim(initialFit)[1], ".\n\n")
          tuneSeq <-  as.data.frame(initialFit)
        } else tuneSeq <-  as.data.frame(initialFit[1:len,])
      colnames(tuneSeq) <- ".maxdepth"
      tuneSeq
    }

  rpartTune <- function(data, len)
    {
      library(rpart)
      initialFit <- rpart(
                          .outcome ~ .,
                          data,
                          control = rpart.control(cp = 0))$cptable
      initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE]
      
      if(dim(initialFit)[1] < len)
        {
          tuneSeq <- data.frame(.cp = seq(min(initialFit[, "CP"]), max(initialFit[, "CP"]), length = len))
        } else tuneSeq <-  data.frame(.cp = initialFit[1:len,"CP"])
      colnames(tuneSeq) <- ".cp"
      tuneSeq
    }
  
  rbfTune <- function(data, len, center = TRUE)
    {
      library(kernlab)
      ## this was changed to follow what kernlab does inside of ksvm and rvm:
      sigmaEstimate <- try(
                           sigest(.outcome ~ ., data, na.action = na.omit, scaled = TRUE),
                           silent = TRUE)
      if(!(class(sigmaEstimate) == "try-error"))
        {
          out <- if(center) sum(sigmaEstimate)/2 else sigmaEstimate[2]
        } else out <- 10 ^((1:len) - 3)
      out
    }


  
  rbfTune2 <- function(data, len, center = TRUE)
    {
      library(kernlab)
      ## this was changed to follow what kernlab does inside of ksvm and rvm:
      sigmaEstimate <- try(
                           sigest(.outcome ~ ., data, na.action = na.omit, scaled = TRUE),
                           silent = TRUE)
      if(!(class(sigmaEstimate) == "try-error"))
        {
          out <- seq(sigmaEstimate[1], sigmaEstimate[3], length = len)
        } else out <- 10 ^((1:len) - 3)
      1/out
    }  
  
  marsSeq <- function(data, len)
    {
      library(earth)
      maxTerms <- if(is.factor(data$.outcome))
        {
          library(mda)
          nrow(
               fda(
                   .outcome~.,
                   data,
                   method = earth,
                   pmethod = "none")$fit$dirs) - 1
        } else nrow(
                    earth(
                          .outcome~.,
                          data,
                          pmethod = "none")$dirs)

      maxTerms <- min(200, floor(maxTerms * .75) + 2)
      unique(
             floor(
                   seq(2, to = maxTerms, length = len)))
    }   


  ## We should not have mtry be larger than the number of features
  ## in the data.
  rfTune <- function(data, len)
    {
      library(randomForest)
      p <- dim(data)[2] - 1 
      if(len == 1) {  
        tuneSeq <- if(!is.factor(data$.outcome)) max(floor(p/3), 1) else floor(sqrt(p))
      } else {
        if(p <= len)
        { 
          tuneSeq <- floor(seq(2, to = p, length = p))
        } else {
          if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
          else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
        }
      }
      if(any(table(tuneSeq) > 1))
        {
          tuneSeq <- unique(tuneSeq)
          cat(
              "note: only",
              length(tuneSeq),
              "unique complexity parameters in default grid.",
              "Truncating the grid to",
              length(tuneSeq), ".\n\n")      
        }
      data.frame(.mtry = tuneSeq)
    }

  cforestTune <- function(data, len)
    {
      p <- dim(data)[2] - 1 
      if(p <= len)
        { 
          tuneSeq <- floor(seq(2, to = p, length = p))
        } else {
          tuneSeq <- seq(from = 2, by = 2, length.out = len)

        }
      if(any(table(tuneSeq) > 1))
        {
          tuneSeq <- unique(tuneSeq)
          cat(
              "note: only",
              length(tuneSeq),
              "unique complexity parameters in default grid.",
              "Truncating the grid to",
              length(tuneSeq), ".\n\n")      
        }
      data.frame(.mtry = tuneSeq)
    }   
  
  ## We fit an initial model to get the range thresholds for these data and
  ## create the grid from these.
  pamTune <- function(data, len)
    {
      library(pamr)
      data <- data[complete.cases(data),,drop = FALSE]
      train.x <- data[!(names(data) %in% ".outcome")]
      train.y <- data[,".outcome"]
      initialThresh <- pamr.train(list(x=t(train.x), y=train.y))$threshold
      initialThresh <- initialThresh[-c(1, length(initialThresh))]
      tuneSeq <- data.frame(
                            .threshold = seq(
                              from = min(initialThresh),
                              to = max(initialThresh), length = len))
      ## pamr.train prints out cv iterations without a line break
      cat("\n")         
      tuneSeq
    }

  larsTune <- function(data, len)
    {
      p <- dim(data)[2] - 1 
      if(p <= len)
        { 
          tuneSeq <- floor(seq(2, to = p, length = p))
        } else {
          if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
          else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
        }
      if(any(table(tuneSeq) > 1))
        {
          tuneSeq <- unique(tuneSeq)
          cat(
              "note: only",
              length(tuneSeq),
              "unique complexity parameters in default grid.",
              "Truncating the grid to",
              length(tuneSeq), ".\n\n")      
        }
      data.frame(.step = tuneSeq)
    }

  roccTune <- function(data, len)
    {
      tmp <- rfTune(data, len)
      names(tmp) <- ".xgenes"
      tmp
    }

  relaxoGrid <- function(data, len)
    {
      library(relaxo)
      tmp <- relaxo(as.matrix(data[, names(data) != ".outcome", drop = FALSE]),
                    data$.outcome)
      expand.grid(.phi = seq(0.1, 0.9, length = len),
                  .lambda = 10^seq(log10(min(tmp$lambda)), log10(quantile(tmp$lambda, probs = .9)), length = len))
    }

  lvqGrid <- function(data, len)
    {
      p <- ncol(data) - 1
      ng <- length(levels(data$.outcome))
      n <- nrow(data)
      tmp <- min(round(0.4*ng*(ng-1 + p/2),0), n)
      out <- expand.grid(.size = unique(floor(seq(tmp, 2*tmp, length = len))),
                         .k = -4 + (1:len)*5)
      out <- subset(out, .k <= .size & .size < n)
      out
    }

  c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
  
  if(!is.null(pp) & method %in% c("hda", "rf", "qrf", "Boruta", "ORFridge",
                                  "ORFpls", "ORFsvm", "ORFlog", "lvq", "rpart",
                                  "rpart2", "pam", "bagEarth", "bagFDA", "earth",
                                  "fda", "svmRadial", "rvmRadial", "lssvmRadial", 
                                  "gaussprRadial", "cforest", "relaxo", "sparseLDA",
                                  "lars2", "smda", "rocc", "foba", "bag", "RRFglobal",
                                  "RRF", "lda2", "extraTrees", "rpartCost", "svmRadialWeights",
                                  "adaboost"))
  {
    pp$method <- pp$options
    pp$options <- NULL
    if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
    pp$ICAcomp <- NULL          
    y <- data$.outcome
    data$.outcome <- NULL
    pp$x <- data
    ppObj <- do.call("preProcess", pp)
    ppObj$call <- "scrubed"
    data <- predict(ppObj, data)
    data$.outcome <- y
  }
  
  trainGrid <- switch(method,
                      nnet =, pcaNNet = expand.grid(
                                .size = ((1:len) * 2) - 1, 
                                .decay = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      avNNet =expand.grid(
                                .size = ((1:len) * 2) - 1, 
                                .decay = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                .bag = FALSE),
                      hda = expand.grid(
                        .gamma = seq(0, 1, length = len), 
                        .lambda =  seq(0, 1, length = len),
                        .newdim = 2:(min(len, ncol(data)))),
                      rda = expand.grid(
                        .gamma = seq(0, 1, length = len), 
                        .lambda =  seq(0, 1, length = len)),
                      gbm = expand.grid(
                        .interaction.depth = seq(1, len),
                        .n.trees = floor((1:len) * 50),
                        .shrinkage = .1),
                      rf =, rfNWS =, rfLSF =, parRF =, qrf =, Boruta =, ORFridge =,
                            ORFpls =, ORFsvm =, ORFlog = rfTune(data, len),
                      gpls = data.frame(.K.prov =seq(1, len) ),
                      lvq = lvqGrid(data, len),
                      rpart = rpartTune(data, len),
                      rpart2 = rpart2Tune(data, len),
                      pcr =, simpls =, widekernelpls =, pls =, kernelpls =, plsTest =,PLS = data.frame(.ncomp = seq(1, min(dim(data)[2] - 1, len), by = 1)),
                      pam = pamTune(data, len),
                      knn = data.frame(.k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0]),
                      nb = data.frame(.usekernel = c(TRUE, FALSE), .fL = 0),
                      multinom = data.frame(.decay = c(0, 10 ^ seq(-4, -1, length = len - 1))),
                      bagEarth =, bagFDA =, earth =,
                      earthTest =, mars =,
                      fda = expand.grid(.degree = 1, .nprune = marsSeq(data, len)),
                      gcvEarth = expand.grid(.degree = 1),
                      svmLinear = data.frame(.C = 2 ^((1:len) - 3)), 
                      svmradial =, svmRadial = expand.grid(
                                     .sigma = rbfTune(data, len),
                                     .C = 2 ^((1:len) - 3)),   
                      svmRadialCost = data.frame(.C = 2 ^((1:len) - 3)), 
                      svmpoly =, svmPoly = expand.grid(
                                   .degree = seq(1, min(len, 3)),      
                                   .scale = 10 ^((1:len) - 4),
                                   .C = 2 ^((1:len) - 3)),
                                        # For 4 different data sets using rvm, I've seen that the default sigma
                                        # causes numerical issue in chol.default (leading minor is not
                                        # positive definite), so we'll use the high value from sigest
                      rvmRadial = data.frame(.sigma = rbfTune(data, len, FALSE)),
                      lssvmRadial =, gaussprRadial = data.frame(.sigma = rbfTune(data, len)),  
                      rvmPoly =, lssvmPoly =, gaussprPoly = expand.grid(
                                                .degree = seq(1, min(len, 3)),      
                                                .scale = 10 ^((1:len) - 4)),
                      glmboost = data.frame(
                        .mstop = floor((1:len) * 50), 
                        .prune = "no"),  
                      gamboost = data.frame(
                        .mstop = floor((1:len) * 50), 
                        .prune = "no"),           
                      blackboost = expand.grid(
                        .maxdepth  = seq(1, len),
                        .mstop = floor((1:len) * 50)),   
                      ada = expand.grid(
                        .iter = floor((1:len) * 50),
                        .maxdepth = seq(1, len),         
                        .nu = .1),
                      cforest = cforestTune(data, len),
                      ctree = data.frame(.mincriterion = seq(from = .99, to = 0.01, length = len)),
                      ctree2 = data.frame(.maxdepth = 1:len),
                      enet = expand.grid(
                        .lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                        .fraction = seq(0.05, 1, length = len)),
                      ridge = data.frame(.lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      lasso = expand.grid(.fraction = seq(.1, .9, length = len)),
                      glmnet = expand.grid(
                        .alpha = seq(0.1, 1, length = len),
                        .lambda = seq(.1, 3, length = 3 * len)),
                      relaxo = relaxoGrid(data, len),
                      logitBoost = data.frame(.nIter =  floor((1:len) * 50)),
                      J48 = data.frame(.C = 0.25),
                      M5Rules = data.frame(.pruned = c("Yes", "No"), .smoothed = "Yes"),
                      M5 = expand.grid(.pruned = c("Yes", "No"), .smoothed = c("Yes", "No"), .rules = c("Yes", "No")),
                      LMT = data.frame(.iter = (1:len) * 20),
                      JRip = data.frame(.NumOpt = 1:len),
                      superpc = expand.grid(.n.components = 1:3,
                        .threshold = seq(.1, .9, length = len)),
                      ppr = data.frame(.nterms = 1:len),
                      sparseLDA = expand.grid(
                        .NumVars = rfTune(data, len)[,1],
                        .lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      penalized = expand.grid(.lambda1 = 2^((1:len) -1),
                        .lambda2 = 2^((1:len) -1)),
                      spls = expand.grid(.K = 1:len, .eta = seq(.1, .9, length = len), .kappa = .5),
                      sda = data.frame(.diagonal = FALSE, .lambda = seq(0, 1, length = len)),
                      mda = data.frame(.subclasses = (1:len) + 1),
                      pda = data.frame(.lambda = 1:len),
                      pda2 = data.frame(.df = 2* (0:(len - 1) + 1)),
                      lars = expand.grid(.fraction = seq(0.05, 1, length = len)),
                      lars2 = larsTune(data, len),
                      PART = data.frame(.threshold = 0.25, .pruned = "yes"),
                      vbmpRadial = data.frame(.estimateTheta = "yes"),
                      smda = expand.grid(
                        .NumVars = rfTune(data, len)[,1],
                        .R = (1:len) + 1,
                        .lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      obliqueTree = expand.grid(
                        .oblique.splits = c("only", "on", "off"),
                        .variable.selection = c("none", "model.selection.aic", "lasso.aic")),
                      nodeHarvest = expand.grid(
                        .maxinter = 1:len,
                        .mode = c("mean", "outbag")),
                      stepLDA =, stepQDA = data.frame(.maxvar = Inf, .direction = "both"),
                      plr = expand.grid(
                        .cp = "bic", 
                        .lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      rocc = roccTune(data, len),
                      foba = expand.grid(
                        .lambda = 10 ^ seq(-5, -1, length = len),
                        .k = larsTune(data, len)[,1]),
                      partDSA = expand.grid(.cut.off.growth = 1:10, .MPD = .1),
                      icr = data.frame(.n.comp = 1:len),
                      neuralnet = expand.grid(.layer1 = ((1:len) * 2) - 1, .layer2 = 0, .layer3 = 0),
                      bag = data.frame(.vars = ncol(data) - 1),
                      hdda = expand.grid(.model = c("best", "dbest"), .threshold = seq(0.05, .3, length = len)),
                      logreg = expand.grid(.ntrees = (1:3) + 1, .treesize = 2^(1+(1:len))),
                      logicBag = expand.grid(.ntrees = (1:len) + 1, .nleaves = 2^((1:len) + 6)),
                      gam = expand.grid(.select = c(TRUE, FALSE), .method = "GCV.Cp"),
                      gamLoess = expand.grid(.span = .5, .degree = 1:2),
                      gamSpline = expand.grid(.df = seq(1, 3, length = len)),
                      plsGlmBinomial =, plsGlmGaussian =, plsGlmGamma =, plsGlmPoisson = data.frame(.nt = 1:len),
                      qrnn = expand.grid(
                                .n.hidden = ((1:len) * 2) - 1, 
                                .penalty = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                .bag = FALSE),
                      cubist = expand.grid(.committees = c(1, 10, 20), .neighbors = c(0, 5, 9)),
                      bstTree = expand.grid(.maxdepth = seq(1, len), .mstop = floor((1:len) * 50), .nu = .1),
                      bstLs =, bstSm =  expand.grid(.mstop = floor((1:len) * 50), .nu = .1),
                      rrlda = expand.grid(.lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                          .hp = seq(.5, 1, length = len),
                                          .penalty = "L2"),
                      leapForward =, leapBackward =, leapSeq = data.frame(.nvmax = 1:len),
                      evtree = data.frame(.alpha = seq(0, 1, length = len)),
                      PenalizedLDA = data.frame(.lambda = 10 ^ seq(-1, -4, length = len), .K = length(levels(data$.outcome)) - 1),
                      xyf =, bdk = somDims(len),
                      rFerns = data.frame(.depth = unique(floor(seq(1, 16, length = len)))),
                      mlp = data.frame(.size =  ((1:len) * 2) - 1),
                      mlpWeightDecay = data.frame(.size =  ((1:len) * 2) - 1, .decay = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      rbf = data.frame(.size =  ((1:len) * 2) + 9),
                      rbfDDA = data.frame(.negativeThreshold =  10 ^(-(1:len))),
                      RRFglobal = expand.grid(.mtry = if (!is.null(data$.outcome) && !is.factor(data$.outcome)) max(floor((ncol(data) - 1)/3), 1) else floor(sqrt((ncol(data)-1))),
                                              .coefReg = seq(0.01, 1, length = len)),
                      RRF = expand.grid(.mtry = if (!is.null(data$.outcome) && !is.factor(data$.outcome)) max(floor((ncol(data) - 1)/3), 1) else floor(sqrt((ncol(data)-1))),
                                        .coefReg = seq(0.01, 1, length = len),
                                        .coefImp = seq(0, 1, length = len)),
                      krlsRadial = expand.grid(.lambda = NA, .sigma = rbfTune2(data, len)),
                      krlsPoly = expand.grid(.lambda = NA, .degree = 1:3),
                      lda2 = data.frame(.dimen = 1:min(ncol(data)-1, length(levels(data$.outcome)) - 1)),
                      C5.0 = expand.grid(.trials = c5seq, .model = c("tree", "rules"), .winnow = c(TRUE, FALSE)),
                      extraTrees = expand.grid(.mtry = rfTune(data, len)[,1], .numRandomCuts = 1:len),
                      kknn = data.frame(.kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0], .distance = 2, .kernel = "optimal"),
                      RFlda = data.frame(.q = 1:len),
                      protoclass = data.frame(.eps = 1:len, .Minkowski = 2),
                      rpartCost = expand.grid(.cp = rpartTune(data, len)$.cp, .Cost = 1:len),
                      svmRadialWeights = expand.grid(.sigma = rbfTune(data, len),
                                                     .C = 2 ^((1:len) - 3),
                                                     .Weight = 1:len),  
                      C5.0Cost = expand.grid(.trials = c5seq, 
                                             .model = c("tree", "rules"), 
                                             .winnow = c(TRUE, FALSE),
                                             .Cost = 1:len),
                      adabag = data.frame(.cp = 0),
                      adaboost = expand.grid(.mfinal = (1:3)*10,
                                             .cp = rpartTune(data, len)$.cp,
                                             .coeflearn = c("Breiman", "Freund", "Zhu")),
                      lda =, lm =, treebag =, sddaLDA =, sddaQDA =,
                      glm =, qda =, OneR =, rlm =, lrm =,
                      rvmLinear =, lssvmLinear =, gaussprLinear =,
                      glmStepAIC =, lmStepAIC =, slda =, Linda =, QdaCov =,
                      C5.0Tree =, C5.0Rules =, bayesglm =, Mlda =,
                      glmrob =, logforest = data.frame(.parameter = "none"))
  trainGrid
}



