"varImp.randomForest" <- function(object, ...)
{
   library(randomForest)
   varImp <- importance(object)
   if(object$type == "regression")
      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
      else {
         retainNames <- levels(object$y)
         varImp <- varImp[, retainNames]
      }
    
   out <- as.data.frame(varImp)
   if(dim(out)[2] == 2)
   {
      tmp <- apply(out, 1, mean)
      out[,1] <- out[,2] <- tmp  
   }
   out
}
