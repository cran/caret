varImp.mvr <- function(object, ...)
{
   library(pls)
   modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
   perf <- MSEP(object)$val
   
   numResp <- dim(modelCoef)[2]
   
   if(numResp <= 2)
   {
      modelCoef <- modelCoef[,1,,drop = FALSE]
      perf <- perf[,1,]
      delta <- -diff(perf)
      delta <- delta/sum(delta)
      out <- data.frame(
         Overall = apply(abs(modelCoef), 1, weighted.mean, w = delta))
      
   } else {
      
      perf <- -t(apply(perf[1,,], 1, diff))
      perf <- t(apply(perf, 1, function(u) u/sum(u)))
      
      out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])
      
      for(i in 1:numResp)
      {
         tmp <- abs(modelCoef[,i,, drop = FALSE])
         out[,i] <- apply(tmp, 1,  weighted.mean, w = perf[i,])
      }
      colnames(out) <- dimnames(modelCoef)[[2]]
      rownames(out) <- dimnames(modelCoef)[[1]]
      
   }
   as.data.frame(out)
}

