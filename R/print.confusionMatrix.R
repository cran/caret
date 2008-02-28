print.confusionMatrix <- function(x, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...)
{
   cat("Confusion Matrix and Statistics\n\n") 
   print(x$table, ...)
      
#   cat("\n(columns are reference results, rows are predictions)\n")

   if(printStats)
   {
 
      overall <- signif(x$overall, digits = digits)
      accCI <- paste(
                     "(",
                     paste(
                           overall[ c("AccuracyLower", "AccuracyUpper")],
                           collapse = ", "),
                     ")",
                     sep = "")      
   
      overallText <- c(
                       paste(overall["Accuracy"]),
                       accCI,
                       paste(overall[c("AccuracyNull", "AccuracyPValue")]),
                       "",
                       paste(overall["Kappa"]))

      overallNames <- c("Accuracy", "95% CI",
                        "No Information Rate",
                        "P-Value [Acc > NIR]",
                        "",
                        "Kappa")
                        
      if(dim(x$table)[1] > 2)
      {
         cat("\nOverall Statistics\n")
         overallNames <- ifelse(
                                overallNames == "",
                                "",
                                paste(overallNames, ":"))
         out <- cbind(
                      format(overallNames, justify = "right"),
                      overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))
         
         print(out, quote = FALSE)
         
         cat("\nStatistics by Class:\n\n")
         print(x$byClass, digits = digits)
         
      } else {
      
         overallText <- c(
                      overallText,
                      "",
                      paste(
                            signif(x$byClass, digits = digits)))
         overallNames <- c(
                           overallNames,
                           "",
                           names(x$byClass))
         overallNames <- ifelse(
                                overallNames == "",
                                "",
                                paste(overallNames, ":"))
         out <- cbind(
                      format(overallNames, justify = "right"),
                      overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))
         
         print(out, quote = FALSE)
      }

        
   }
   invisible(x)   
}
