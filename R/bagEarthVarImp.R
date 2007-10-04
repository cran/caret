varImp.bagEarth <- varImp.classbagg <- function(object, ...)
{
   allImp <- lapply(object$fit, varImp, ...)
   impDF <- as.data.frame(allImp)
   meanImp <- apply(impDF, 1, mean)
   out <- data.frame(Overall = meanImp)
   rownames(out) <- names(meanImp)
   out
}
