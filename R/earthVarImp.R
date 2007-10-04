varImp.earth <- function(object, value = "grsq", ...)
{

   valNames <- c("grsq", "rsq", "rss", "gcv")
   if(all(!(value %in% valNames))) stop(paste("value must be one of:", paste(valNames, collapse = ", ")))

   splits <- object$cuts[object$selected.terms[-1],]
   
   allNames <- colnames(object$dirs)
   varNames <- apply(splits, 1, function(u) names(which(u != 0)))
   
   perf <- switch(
      value,
      grsq = diff(earth:::get.rsq(object$gcv.per.subset[object$selected.terms], object$gcv.per.subset[1])),
      rsq = diff(earth:::get.rsq(object$rss.per.subset[object$selected.terms], object$rss.per.subset[1])),
      rss = -diff(object$rss.per.subset[object$selected.terms]),
      gcv = -diff(object$gcv.per.subset[object$selected.terms]))

   
   out <- data.frame(Overall = rep(0, dim(object$dirs)[2]))
   rownames(out) <- allNames
   
   for(i in seq(along = varNames))
   {
      features <- which(allNames %in% varNames[[i]])
      out$Overall[features] <- out$Overall[features] + perf[i]
   }
   
   tol <- min(0.0000000001, perf[perf>0]/10)
   if(any(out$Overall < 0)) out$Overall[out$Overall < 0] <- tol
   
   out
}

