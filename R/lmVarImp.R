varImp.lm <- function(object, ...) 
{
   varImps <-  abs(summary(object)$coef[-1, "t value"])
   out <- data.frame(varImps)
   colnames(out) <- "Overall"
   if(!is.null(names(varImps))) rownames(out) <- names(varImps)
   out   
}
