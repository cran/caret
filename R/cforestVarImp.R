varImp.RandomForest <- function(object, normalize = TRUE, ...)
{
   library(party)
   variableImp <- varimp(object, ...)

   impValues <- variableImp[, "MeanDecreaseAccuracy"]
   
   if(normalize & any(variableImp[, "Standard.Deviation"] < .Machine$double.eps))
   {
      warning("zero standard deviation values, changed normalize to FALSE")
      normalize <- FALSE
   }
   if(normalize) impValues <- impValues/variableImp[, "Standard.Deviation"]
   
   out <- data.frame(Overall = impValues)
   rownames(out) <- rownames(variableImp)

   out
}

