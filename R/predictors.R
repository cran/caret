"predictors" <- function(x, ...){
    UseMethod("predictors")
  }

predictors.train <- function(x, ...)
  {
    out <- predictors(x$finalModel)
    if(all(is.na(out))) out <- x$xNames
    out
  }

hasTerms <- function(x)
  {
    objNames <- c(names(x), slotNames(x))
    "terms" %in% tolower(objNames)
  }

## basicVars tries to detect the actual variable that are used
## when a formula might include other terms (such as interactions)
## For example:
## > x
## [1] "medv" "crim" "zn"   "age" 
## > y
## [1] "crim"     "I(age^2)" "zn"   
## > basicVars(x, y)
## [1] "crim" "zn"   "age"

basicVars <- function(x, y)
  {
    hasVar <- rep(NA, length(x))
    for(i in seq(along = x))
      hasVar[i] <- length(grep(x[i], y, fixed = TRUE)) > 0
    x[hasVar] 
  }

predictors.terms <- function(x, ...)
  {
    if(is.null(x)) return(NA)
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.formula <- function(x, ...)
  {
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.list <- function(x, ...)
  {
    out <- lapply(x, predictors)
    names(out) <- names(x)
    out
  }

predictors.mvr <- function(x, ...)
  {
    rownames(x$projection)
  }

predictors.gbm <- function(x, ...)
  {

    library(gbm)
    varList <- if(hasTerms(x)) predictors(x$terms) else colnames(x$data$x.order)
    relImp <- summary(x, plotit = FALSE)
    varUsed <- as.character(subset(relImp, rel.inf != 0)$var)
    basicVars(varList, varUsed)    
  }

predictors.Weka_classifier <- function(x, ...)
{
  ## todo We can do better here by digging in and seeing what
  ## variables were actually used
  predictors(x$terms)
}

predictors.fda <- function(x, ...)
{
  tmp <- predictors(x$terms)
  out <- if(class(x$fit) == "earth") predictors(x$fit) else tmp
  out
  
}

predictors.earth <- function(x, ...)
{
  basicVars(x$namesx.org, rownames(coef(x)))
}

predictors.gausspr <- function(x, ...)
{
  if(hasTerms(x) & !is.null(x@terms))
    {
      out <- predictors.terms(x@terms)
    } else {
      out <- colnames(attr(x, "xmatrix"))
    }
  if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
  if(is.null(out)) out <-NA
  out
  
}

predictors.ksvm <- function(x, ...)
{
  if(hasTerms(x) & !is.null(x@terms))
    {
      out <- predictors.terms(x@terms)
    } else {
      out <- colnames(attr(x, "xmatrix"))
    }
  if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
  if(is.null(out)) out <-NA
  out
  
}

predictors.lssvm <- function(x, ...)
{
  if(hasTerms(x) & !is.null(x@terms))
    {
      out <- predictors.terms(x@terms)
    } else {
      out <- colnames(attr(x, "xmatrix"))
    }
  if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
  if(is.null(out)) out <-NA
  out
  
}

predictors.rvm <- function(x, ...)
{
  if(hasTerms(x) & !is.null(x@terms))
    {
      out <- predictors.terms(x@terms)
    } else {
      out <- colnames(attr(x, "xmatrix"))
    }
  if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
  if(is.null(out)) out <-NA
  out
  
}


predictors.gpls <- function(x, ...)
{
    out <- if(hasTerms(x)) predictors(x$terms) else colnames(x$data$x.order)
    out[!(out %in% "Intercept")]
}

predictors.knn3 <- function(x, ...)
  {
    colnames(x$learn$X)
  }

predictors.knnreg <- function(x, ...)
  {
    colnames(x$learn$X)
  }

predictors.LogitBoost <- function(x, ...)
  {
    if("Weka_classifier" %in% class(x))
      {
        out <- predictors.Weka_classifier(x)
      } else {
        if(!is.null(x$xNames))
          {
            out <- unique(x$xNames[x$Stump[, "feature"]])
          } else out <- NA
      }
    out
  }

predictors.lda <- function(x, ...)
{
    if(hasTerms(x)) predictors(x$terms) else colnames(x$means)
}

predictors.qda <- function(x, ...)
{
    if(hasTerms(x)) predictors(x$terms) else colnames(x$means)
}

predictors.rda <- function(x, ...)
{
    x$varnames
}

predictors.multinom <- function(x, ...)
{
    predictors(x$terms)
}

predictors.nnet <- function(x, ...)
{
    if(hasTerms(x)) predictors(x$terms) else NA
}

predictors.pcaNNet <- function(x, ...)
{
    rownames(x$pc$rotation)
}

predictors.avNNet <- function(x, ...)
{
    rownames(x$pc$rotation)
}

predictors.NaiveBayes <- function(x, ...)
{
    if(hasTerms(x)) predictors(x$terms) else x$varnames
}

predictors.pamrtrained <- function(x, newdata = NULL, threshold = NULL,  ...)
  {
    if(is.null(newdata))
      {
        if(!is.null(x$xData)) newdata <- x$xData else stop("must supply newdata") 
      }
    if(is.null(threshold))
      {
        if(!is.null(x$threshold)) threshold <- x$threshold else stop("must supply threshold") 
      }
    library(pamr)
    varIndex <- pamr.predict(x, newx = newdata, threshold = threshold, type = "nonzero")
    colnames(newdata)[varIndex]
  }

## todo finalize this
predictors.superpc <- function(x, newdata = NULL, threshold = NULL, n.components = NULL, ...)
  {
    NA
  }

predictors.randomForest <- function(x, ...)
{
  ## After doing some testing, it looks like randomForest
  ## will only try to split on plain main effects (instead
  ## of interactions or terms like I(x^2).
  varIndex <- as.numeric(names(table(x$forest$bestvar)))
  varIndex <- varIndex[varIndex > 0]
  varsUsed <- names(x$forest$ncat)[varIndex]
  varsUsed
}

predictors.slda <- function(x, ...)
{
    if(hasTerms(x)) predictors(x$terms) else predictors(x$mylda)
}

predictors.rpart <- function(x, surrogate = TRUE, ...)
{
  out <- as.character(x$frame$var)
  out <- out[!(out %in% c("<leaf>"))]
  if(surrogate)
    {
      splits <- x$splits
      splits <- splits[splits[,"adj"] > 0,]
      out <- c(out, rownames(splits))
    }
  unique(out)
}

predictors.regbagg <- function(x, surrogate = TRUE, ...)
{
  eachTree <- lapply(x$mtree,
                     function(u, surr) predictors.rpart(u$btree, surrogate = surr),
                     surr = surrogate)
  unique(unlist(eachTree))
}

predictors.classbagg <- function(x, surrogate = TRUE, ...)
{
  eachTree <- lapply(x$mtree,
                     function(u, surr) predictors.rpart(u$btree, surrogate = surr),
                     surr = surrogate)
  unique(unlist(eachTree))
}

predictors.lm <- function(x, ...)
{
    predictors(x$terms)
}

predictors.glmboost <- function(x, ...)
  {
    if(is.null(x$data$formula))
      {
        varNames <- colnames(x$data$x)
        varNames <- varNames[!(varNames %in% "(Intercept)")]
        
      } else {
        varNames <- predictors.terms(x$data$formula)
      }
    varNames
  }

predictors.blackboost <- function(x, ...)
  {

    if("menv" %in% slotNames(x$data) && !is.null(x$data@menv@formula$input))
      {
        varNames <- all.vars(x$data@menv@formula$input)
      } else {
        varNames <- unlist(lapply(x$data@inputs@variables, colnames))
      }
    varNames
  }

predictors.gamboost <- function(x, ...)
  {
    if("menv" %in% names(x$data) && !is.null(x$data$menv@formula$input))
      {
        varNames <- all.vars(x$data$menv@formula$input)
      } else {
        varNames <- colnames(x$data$x)
      }
    varNames
  }

predictors.BinaryTree <- function(x, surrogate = TRUE, ...)
  {
    treeObj <- unlist(nodes(x, 1))
    target <- "psplit\\.variableName"
    vars <- treeObj[grep(target, names(treeObj))]
    if(surrogate)
      {
        target2 <- "ssplits\\.variableName"
        svars <- treeObj[grep(target, names(treeObj))]
        vars <- c(vars, svars)

      }
    unique(vars)
  }


predictors.bagEarth <- function(x, ...)
  {
    eachFit <- lapply(x$fit, predictors.earth)
    unique(unlist(eachFit))
  }

predictors.bagFDA <- function(x, ...)
  {
    eachFit <- lapply(x$fit, predictors.fda)
    unique(unlist(eachFit))
  }

predictors.ppr <- function(x, ...)
  {
    x$xnames
  }

predictors.spls <- function(x, ...)
  {
    colnames(x$x)[x$A]
  }

predictors.splsda <- function(x, ...)
  {
    colnames(x$x)[x$A]
  }

predictors.mvr <- function(x, ...)
  {
    rownames(x$coeff)
  }

predictors.glm <- function(x, ...)
{
    predictors(x$terms)
}

predictors.mda <- function(x, ...)
{
    predictors(x$terms)
}


predictors.glmnet <- function(x, lambda = NULL, ...)
{
  library(glmnet)
    if(is.null(lambda))
      {
        if(!is.null(x$lambdaOpt))
          {
            lambda <- x$lambdaOpt
          } else stop("must supply a vaue of lambda")
      }
    out <- coef(x, s = lambda)[,1]
    out <- names(out)[out != 0]
    out[out != "(Intercept)"]
}

predictors.penfit <- function(x, ...)
  {
    library(penalized)
    out <- coef(x, "all")
    out <- names(out)[out != 0]
    out[out != "(Intercept)"]
  }

predictors.lars <- function(x, s = NULL, ...)
{
  library(lars)
  if(is.null(s))
    {
      if(!is.null(x$tuneValue))
        {
          s <- x$tuneValue$.fraction
        } else stop("must supply a vaue of s")
      out <- predict(x, s = s,
                     type = "coefficients",
                     mode = "fraction")$coefficients

    } else {
      out <- predict(x, s = s, ...)$coefficients

    }
  names(out)[out != 0]
}

predictors.enet <- function(x, s = NULL, ...)
{
  library(elasticnet)
  if(is.null(s))
    {
      if(!is.null(x$tuneValue))
        {
          s <- x$tuneValue$.fraction
        } else stop("must supply a vaue of s")
      out <- predict(x, s = s,
                     type = "coefficients",
                     mode = "fraction")$coefficients

    } else {
      out <- predict(x, s = s, ...)$coefficients

    }
  names(out)[out != 0]
}

predictors.sda <- function(x, ...)
  {
    out <- x$varNames
    if(is.null(out)) out <- varIndex
    out
  }

predictors.smda <- function(x, ...)
  {
    out <- x$varNames
    if(is.null(out)) out <- varIndex
    out
  }

predictors.stepclass <- function(x, ...)
  {
    form <- x$formula
    form[[2]] <- NULL
    all.vars(form)
  }


predictors.trocc <- function(x, ...) x$genes


predictors.foba <- function(x, k = NULL, ...)
  {
    if(is.null(k))
      {
        if(!is.null(x$tuneValue)) k <- x$tuneValue$.k[1]  else stop("Please specify k")
      }
    library(foba)
    names(predict(x, k = k, type = "coefficients")$selected.variables)
  }

predictors.dsa <- function(x, cuts = NULL, ...)
  {
    if(is.null(cuts) & !is.null(x$tuneValue))
      {
        cuts <- x$tuneValue$.cut.off.growth[1]
      } else {
        if(is.null(cuts)) stop("please supply a value for 'cuts'")
      }
    tmp <- x$var.importance[,cuts]
    names(tmp)[which(tmp != 0)]

  }

predictors.RandomForest <- function(x, ...)
  {
    library(party)
    vi <- varimp(x, ...)
    names(vi)[vi != 0]
  }


predictors.logreg <- function(x, ...)
  {
    getVarIndex <- function(y) unique(y$trees$knot)
    varNums <- unique(unlist(lapply(x$model$trees, getVarIndex)))
    varNums <- varNums[varNums > 0]
    if(length(varNums) > 0) colnames(x$binary)[varNums] else NA
  }

predictors.logforest <- function(x, ...)
  {
    varNums <- sort(unique(unlist(lapply(x$AllFits, predictors))))
    if(length(varNums) == 0) varNums <- NA
    varNums
  }

predictors.logicBagg <- function(x, ...)
  {
    varNums <- lapply(x$logreg.model,
                      function(y) lapply(y$trees,
                                         function(z) z$trees$knot))
    

    varNums <- sort(unique(unlist(varNums)))
    varNums <- varNums[varNums > 0]
    if(length(varNums) > 0) colnames(x$data)[varNums] else NA    
  }


predictors.gam <- function(x, ...)
  {
    library(mgcv)
    tmp <- varImp(x, scale = FALSE)
    rownames(tmp)[tmp$Overall > 0]
  }

