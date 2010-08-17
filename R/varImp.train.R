"varImp.train" <-
  function(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...)
{

  if(is.null(object$finalModel)) stop("must have retained the final model")
  modelName <- object$method    
  if(!(class(object$finalModel)[1] %in% gsub("varImp.", "", as.character(methods("varImp"), fixed = TRUE)))) useModel <- FALSE   
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
                      library(pamr)
                      if(is.null(object$finalModel$xData)) stop("the train$finalModel object needs an xData object")
                      varImp(
                             object$finalModel, 
                             threshold = ifelse(is.null(object$bestTune), 0, object$bestTune[,1]),
                             data = object$finalModel$xData)         
                    },
                    mars =, earth = 
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
