"varImp.train" <-
function(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...)
{

   if(is.null(object$finalModel)) stop("must have retained the final model")
   modelName <- object$method    
   if(!(modelName %in% c("lm", "pls", "pam", "rf", "rpart", "gbm", "treebag", "mars", "earth", "bmars", "cforest"))) useModel <- FALSE   
   availMethods <- methods("varImp")
   availMethods <- rownames(attributes(availMethods)$info)
   availMethods <- gsub("varImp.", "", availMethods, fixed = TRUE)
   
   hasMethod <- any(class(object$finalModel) %in% availMethods)
   if(!hasMethod & useModel) useModel <- FALSE
   
   if(useModel)
   {  
      imp <- switch(
         class(object$finalModel)[1],
         pamrtrained = 
         {
            if(is.null(object$finalModel$xData)) stop("the train$finalModel object needs an xData object")
            varImp(
               object$finalModel, 
               threshold = ifelse(is.null(object$tuneValue), 0, object$tuneValue),
               data = object$finalModel$xData)         
         },
         mars = 
         {
            if(is.null(object$trainingData)) stop("the train object needs an trainingData object")
            tmp <- object$trainingData[,names(object$trainingData) != ".outcome"]
            varImp(object$finalModel, data = tmp, ...)
         },
         varImp(object$finalModel, ...))
   } else {
      isX <- which(!(dimnames(object$trainingData)[[2]] %in% ".outcome"))
      imp <- filterVarImp(
         object$trainingData[, isX],
         object$trainingData[, -isX],         
         nonpara = nonpara,
         ...)   
      modelName <- ifelse(is.factor(object$trainingData[, -isX]),
         "ROC curve",
         ifelse(nonpara, "loess r-squared", "Linear model"))      
   }
   
   

   if(scale)
   {
      if(class(object$finalModel)[1] == "pamrtrained") imp <- abs(imp)
      imp <- imp - min(imp, na.rm = TRUE) 
      imp <- imp/max(imp, na.rm = TRUE)*100      
   }
   out <- list(importance = imp,
      model = modelName,
      calledFrom = "varImp")
   
   structure(out, class = "varImp.train")
}
