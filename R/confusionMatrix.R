confusionMatrix <- 
function(data, ...){
   UseMethod("confusionMatrix")
}

confusionMatrix.default <- function(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), ...)
{
   library(e1071)
   if(!is.factor(data)) data <- factor(data)
   if(!is.factor(reference)) data <- factor(reference)

   if(length(levels(data)) != length(levels(reference)))
      stop("the data and reference factors must have the same number of levels")
   
   if(any(levels(data) != levels(reference)))
      stop("the data and reference values must have exactly the same levels")
     
   classLevels <- levels(data)
   numLevels <- length(classLevels)
   if(numLevels < 2) 
      stop("there must be at least 2 factors levels in the data")
   
   if(numLevels == 2 & is.null(positive))  positive <- levels(reference)[1]
   
   classTable <- table(data, reference, dnn = dnn, ...)
   
   overall <- unlist(classAgreement(table(reference, data)))[c("diag", "kappa")]
   names(overall) <- c("Accuracy", "Kappa")  
             
   if(numLevels == 2)
   {
      negative <- levels(data)[which(levels(data) != positive)]
      tableStats <- basic2x2Stats(data, reference, positive, negative)
   } else {
      tableStats <- NULL
      for(i in classLevels)
      {
         newData <- factor(ifelse(data == i, i, "other"), levels = c(i, "other"))
         newRef  <- factor(ifelse(reference == i, i, "other"), levels = c(i, "other"))
         tableStats <- rbind(tableStats, basic2x2Stats(newData, newRef, i, "other")) 
      }
      rownames(tableStats) <- paste("Class:", classLevels)
   
   }

   structure(list(
      positive = positive,
      table = classTable, 
      overall = overall, 
      byClass = tableStats,
      dots = list(...)), 
      class = "confusionMatrix")
}


