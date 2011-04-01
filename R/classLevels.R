"classLevels" <-   function(x, ...) UseMethod("classLevels")

classLevels.default <- function(x, ...) NULL

classLevels.train <- function(x, ...) classLevels(x$finalModel)

classLevels.ksvm <- function(x, ...) lev(x)
classLevels.lssvm <- function(x, ...) lev(x)
classLevels.gausspr <- function(x, ...) lev(x)

classLevels.LdaClassic <- function(x, ...) names(x@prior)
classLevels.QdaClassic <- function(x, ...) names(x@prior)

classLevels.BinaryTree <- function(x, ...) levels(x@data@get("response")[,1])
classLevels.RandomForest <- function(x, ...) levels(x@data@get("response")[,1])

classLevels.lda <- function(x, ...) names(x$prior)
classLevels.lda <- function(x, ...) names(x$prior)

classLevels.rda <- function(x, ...) names(x$prior)

classLevels.gbm <- function(x, ...) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

classLevels.randomForest <- function(x, ...) x$classes

classLevels.nnet <- function(x, ...) x$coefnames

classLevels.pcaNNet <- function(x, ...) x$model$coefnames

classLevels.gpls <- function(x, ...) x$model$levs

classLevels.rpart <- function(x, ...) attr(x, "ylevels")

classLevels.plsda <- function(x, ...) x$obsLevels

classLevels.pamrtrained <- function(x, ...) names(x$prior)

classLevels.knn3 <- function(x, ...) levels(x$learn$y)

classLevels.NaiveBayes <- function(x, ...) x$levels

classLevels.earth <- function(x, ...) x$levels

classLevels.fda <- function(x, ...) names(x$prior)

classLevels.bagFDA <- function(x, ...) x$levels

classLevels.classbag <- function(x, ...) levels(x$y)

classLevels.glm <- function(x, ...) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

classLevels.glmboost <- function(x, ...) levels(x$response)
classLevels.gamboost <- function(x, ...) levels(x$response)
classLevels.blackboost <- function(x, ...) levels(x$response)

classLevels.glmnet <- function(x, ...) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

classLevels.sdda <- function(x, ...) rownames(x$means)

classLevels.LogitBoost <- function(x, ...) x$lablist

classLevels.LogitBoost <- function(x, ...) x$lablist

classLevels.Weka_classifier <- function(x, ...) x$levels

classLevels.slda <- function(x, ...) names(x$mylda$prior)

classLevels.sda <- function(x, ...)
  {
    ## objects from package sparseLDA and sda have the
    ## same class name
    if(any(names(x) == "regularization")) names(x$prior) else x$classes
  }

classLevels.splsda <- function(x, ...)
  {
    ## objects from package caret and spls have the
    ## same class name, but this works for either
    ilevels(x$y)
  }


classLevels.mda <- function(x, ...) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))

classLevels.VBMP.obj <- function(x, ...) if(any(names(x) == "obsLevels")) x$obsLevels else NULL

classLevels.nodeHarvest <- function(x, ...) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))

classLevels.stepclass <- function(x, ...) classLevels(x$fit)

classLevels.plr <- function(x, ...) unique(unlist(lapply(strsplit(rownames(x$means), ".", fixed = TRUE), function(x) x[1])))



#classLevels.GAMens <- function(x, ...)
#  classLevels.rocc <- function(x, ...)
#  classLevels.partDSA <- function(x, ...)
#  classLevels.hda <- function(x, ...)
#  classLevels.scrda <- function(x, ...)
#  classLevels.plr <- function(x, ...) 
#  classLevels.hdda <- function(x, ...) 
#  classLevels.logreg <- function(x, ...) 
#  classLevels.logforest <- function(x, ...) 
#  classLevels.gam <- function(x, ...) 
#  classLevels.gamLoess <- function(x, ...) 
#  classLevels.gamSpline <- function(x, ...) 
#  classLevels.Boruta <- function(x, ...) 










































