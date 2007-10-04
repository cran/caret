
resampleHist <- function(object, type = "density", ...)
{
   if(object$control$method == "oob") stop("out-of-bag error rate was selected. This plot cannot be created")
   if(is.null(object$resample)) stop("No resample values were found. This plot cannot be created")
  
   results <- stack(object$resample)
   
   if(type == "density")
   {
      out <- densityplot(
         ~ values|ind, 
         data = results, 
         scales = list(relation = "free"),
         xlab = "",
         as.table = TRUE,
         ...)  

   } else {
      out <- histogram(
         ~ values|ind, 
         data = results, 
         scales = list(relation = "free"),
         as.table = TRUE,         
         xlab = "",
         ...)    
   }
   out
}
