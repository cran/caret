ipredStats <- function(x)
{
   # error check
   if(is.null(x$X)) stop("to get OOB stats, keepX must be TRUE when calling the bagging function")
   
   foo <- function(object, y, x)
   {
       holdY <- y[-object$bindx]
       if(is.factor(y))
       {
           library(e1071)
           tmp <- predict(object$btree, x[-object$bindx,], type = "class")
           tmp <- factor(as.character(tmp), levels = levels(y))
           out <- c(
               mean(holdY == tmp),
               classAgreement(table(holdY, tmp))$kappa)
           
       } else {
           tmp <- predict(object$btree, x[-object$bindx,])

           out <- c(
               sqrt(mean((tmp - holdY)^2, na.rm = TRUE)),
               cor(holdY, tmp, use = "pairwise.complete.obs")^2)
       }
       out    
   }
   eachStat <- lapply(x$mtrees, foo, y = x$y, x = x$X)
   eachStat <- matrix(unlist(eachStat), nrow = length(eachStat[[1]]))
   out <- c(
   	apply(eachStat, 1, mean, na.rm = TRUE),
   	apply(eachStat, 1, sd, na.rm = TRUE))
   names(out) <- if(is.factor(x$y)) c("Accuracy", "Kappa", "AccuracySD", "KappaSD") else c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
   out
}


rfStats <- function(x)
{
   out <- switch(
      x$type,
      regression =   c(x$mse[length(x$mse)], x$rsq[length(x$rsq)]),
      classification = {
         library(e1071)
         c(
            1 - x$err.rate[x$ntree, "OOB"],
            classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]])
        })
   names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
   out              
}

cforestStats <- function(x)
{
   library(party)
   
   obs <- x@data@get("response")[,1]
   pred <- predict(x,  x@data@get("input"), OOB = TRUE)
   postResample(pred, obs)
   

}

bagEarthStats <- function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5))

#
#library(ipred)
#library(gbm)
#library(randomForest)
#
#data(GlaucomaM)
#
#GlaucomaM <- GlaucomaM[sample(seq(along = GlaucomaM$Class)),]
#
#
#rfClassFit <- randomForest(Class ~ ., data = GlaucomaM)
#bagClassFit <- bagging(Class ~ ., data = GlaucomaM)
#gbmClassFit <- gbm(ifelse(Class == "glaucoma", 1, 0) ~ ., data = GlaucomaM, distribution = "bernoulli", train.fraction = 0.5)
#
#
#rfStats(rfClassFit)
#ipredStats(bagClassFit)
#
