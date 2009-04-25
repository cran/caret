"createGrid" <-
  function(method, len = 3, data = NULL)
{
  ## rpart needs its own function since we fit an initial model,
  ## read off the possible complexity parameters and use
  ## those values to devleop the grid. 
  rpartTune <- function(data, len)
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
  
  rbfTune <- function(data, len, center = TRUE)
    {
      library(kernlab)
      ## this was changed to follow what kernlab does inside of ksvm and rvm:
      sigmaEstimate <- try(
                           sigest(.outcome ~ ., data, na.action = na.omit, scaled = FALSE),
                           silent = TRUE)
      if(!(class(sigmaEstimate) == "try-error"))
        {
          out <- if(center) sum(sigmaEstimate)/2 else sigmaEstimate[2]
        } else out <- 10 ^((1:len) - 3)
      out
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

  
  trainGrid <- switch(method,
                      nnet =, pcaNNet = expand.grid(
                                .size = ((1:len) * 2) - 1, 
                                .decay = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                      rda = expand.grid(
                        .gamma = seq(0, 1, length = len), 
                        .lambda =  seq(0, 1, length = len)),
                      gbm = expand.grid(
                        .interaction.depth = seq(1, len),
                        .n.trees = floor((1:len) * 50),
                        .shrinkage = .1),
                      rf =, rfNWS =, rfLSF = rfTune(data, len),
                      gpls = data.frame(.K.prov =seq(1, len) ),
                      lvq = data.frame(.k =seq(4, 3+len) ),
                      rpart = rpartTune(data, len),
                      pls =, plsTest =,PLS = data.frame(.ncomp = seq(1, min(dim(data)[2] - 1, len), by = 1)),
                      pam = pamTune(data, len),
                      knn = data.frame(.k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0]),
                      nb = data.frame(.usekernel = c(TRUE, FALSE)),
                      multinom = data.frame(.decay = c(0, 10 ^ seq(-4, -1, length = len - 1))),
                      bagEarth =, bagFDA =, earth =,
                      earthTest =, mars =,
                      fda = expand.grid(.degree = 1, .nprune = marsSeq(data, len)),
                      svmLinear = data.frame(.C = 10 ^((1:len) - 2)), 
                      svmradial =, svmRadial = expand.grid(
                                     .sigma = rbfTune(data, len),
                                     .C = 10 ^((1:len) - 2)),   
                      svmpoly =, svmPoly = expand.grid(
                                   .degree = seq(1, min(len, 3)),      
                                   .scale = 10 ^((1:len) - 4),
                                   .C = 10 ^((1:len) - 2)),
                                        # For 4 different data sets using rvm, I've seen that the default sigma
                                        # causes numerical issue in chol.default (leading minor is not
                                        # positive definite), so we'll use the high value form sigest
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
                      lasso = expand.grid(.fraction = seq(.1, .9, length = len)),
                      glmnet = expand.grid(
                        .alpha = seq(0.1, 1, length = len),
                        .lambda = seq(.1, 3, length = 3 * len)),
                      relaxo = expand.grid(
                        .phi = seq(0.1, 0.9, length = len),
                        .lambda = 10^seq(-1, 3, length = 3 * len)),
                      logitBoost = data.frame(.nIter =  floor((1:len) * 50)),
                      J48 = data.frame(.C = 0.25),
                      M5Rules = data.frame(.pruned = c("Yes", "No")),
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
                      sda = data.frame(.diagonal = FALSE),
                      mda = data.frame(.subclasses = (1:len) + 1),
                      pda = data.frame(.lambda = 1:len),
                      pda2 = data.frame(.df = 2* (0:(len - 1) + 1)),
                                            lars = expand.grid(.fraction = seq(0.05, 1, length = len)),
                      lars2 = larsTune(data, len),
                      PART = data.frame(.threshold = 0.25, .pruned = "yes"), 
                      lda =, lm =, treebag =, sddaLDA =, sddaQDA =,
                      glm =, qda =, OneR =, rlm =,
                      rvmLinear =, lssvmLinear =, gaussprLinear =,
                      lmStepAIC =, slda = data.frame(.parameter = "none"))
  trainGrid
}
