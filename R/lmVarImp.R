varImp.lm <- function(object, ...) 
{
  values <- summary(object)$coef
   varImps <-  abs(values[-1, grep("value$", colnames(values))])
   out <- data.frame(varImps)
   colnames(out) <- "Overall"
   if(!is.null(names(varImps))) rownames(out) <- names(varImps)
   out   
}
