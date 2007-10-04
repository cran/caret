print.confusionMatrix <- function(x, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...)
{
   cat("Confusion Matrix and Statistics\n\n") 
   print(x$table, ...)
      
#   cat("\n(columns are reference results, rows are predictions)\n")

   if(printStats)
   {
      if(dim(x$table)[1] == 2)
      {
         cat("\nStatistics:")
         allStats <- as.matrix(c(x$overall, x$byClass))
         colnames(allStats) <- ""
         print(allStats, digits = digits)
         cat(paste("\nClass =", x$positive, "was used to define a positive result\n"))
      
      } else {
         cat("\nOverall Statistics:\n")
         print(x$overall, digits = digits)   
         cat("\nStatistics by Class:\n")
         print(x$byClass, digits = digits)   
      }
   }
   invisible(x)   
}
