plotClassProbs <- function(object, ...)
{
   classCol <- !(names(object) %in% c("obs", "pred", "model", "dataType"))
   if(sum(classCol) == 2)
   {
      probLabel <- levels(object$obs)[1]
      object$obs <- factor(paste("Data:", object$obs), levels = paste("Data:", levels(object$obs)))
      histForm <- paste("~", names(object)[1], "|")
      if(length(unique(object$dataType)) > 1) histForm <- paste(histForm, "dataType *")
      histForm <- paste(histForm, "obs")
      if(length(unique(object$model)) > 1) histForm <- paste(histForm, "* model")      
      histForm <- as.formula(histForm)
      histLabel <- paste("Probability of", probLabel)
      
      out <- histogram(histForm, object, xlab = histLabel, ...)

   } else {
   
      classCol <- !(names(object) %in% c("obs", "pred", "model", "dataType"))
      stackProbs <- stack(object[, 1:sum(classCol)])
      stackProbs$obs <- rep(
         paste("Data:", object$obs), sum(classCol))
      stackProbs$ind <- paste("Model:", stackProbs$ind)         
      stackProbs$dataType <- rep(object$dataType, sum(classCol))
      stackProbs$model <- rep(object$model, sum(classCol))      
      histForm <- "~ values|ind*obs"
      if(length(unique(object$dataType)) > 1) histForm <- paste(histForm, "* dataType")
      if(length(unique(object$model)) > 1) histForm <- paste(histForm, "* model")      
      histForm <- as.formula(histForm)
      out <- histogram(histForm,  stackProbs, nint = 20, 
         xlab = "Class Probability", as.table = TRUE, ...)
   }

out
}

