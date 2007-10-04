"plot.varImp.train" <-
function(x, top = dim(x$importance)[1],  ...)
{
   library(grid)    
   plotObj <- sortImp(x, top)
          
   if(dim(plotObj)[2] == 2)
   {
      plotObj <- plotObj[,1,drop = FALSE]
      names(plotObj) <- "Importance"
   }
          
          
   featureNames <- dimnames(plotObj)[[1]]            
   outcomeNames <- names(plotObj)
   
   if(dim(plotObj)[2] > 1)
   {            
      stackedData <- stack(plotObj)
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)),
            levels = rev(featureNames))      
      names(stackedData) <- c("Importance", "Class", "Feature")
      
   } else {
      stackedData <- plotObj
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)),
            levels = rev(featureNames))            
      names(stackedData) <- c("Importance", "Feature")
   }
   
   formulaText <- ifelse(dim(plotObj)[2] > 1, "Feature ~ Importance|Class", "Feature ~ Importance")
   
   if(x$model == "pam")
   {
      impSign <- factor(ifelse(stackedData$Importance > 0, "Positive", "Negative"),
         levels = c("Positive", "Negative"))
      stackedData$Importance <- abs(stackedData$Importance)
      impPlot <- dotplot(as.formula(formulaText), stackedData,
         groups = impSign,
         panel = panel.needle, ...)   
   } else {
      impPlot <- dotplot(as.formula(formulaText), stackedData,
      panel = panel.needle, ...)
   }
   

   
   impPlot
}
