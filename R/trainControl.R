trainControl <- function(method = "boot",
                         number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
                         repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
                         p = .75,
                         initialWindow = NULL,
                         horizon = 1,
                         fixedWindow = TRUE,
                         verboseIter = FALSE,
                         returnData = TRUE,
                         returnResamp = "final",
                         savePredictions = FALSE,
                         classProbs = FALSE,
                         summaryFunction = defaultSummary,
                         selectionFunction = "best",
                         custom = NULL,
                         preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
                         index = NULL,
                         indexOut = NULL,
                         timingSamps = 0,
                         predictionBounds = rep(FALSE, 2),
                         allowParallel = TRUE)
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  if(!(returnResamp %in% c("all", "final", "none"))) stop("incorrect value of returnResamp")
  if(length(predictionBounds) > 0 && length(predictionBounds) != 2) stop("'predictionBounds' should be a logical or numeric vector of length 2")
  if(any(names(preProcOptions) == "method")) stop("'method' cannot be specified here")
  if(any(names(preProcOptions) == "x")) stop("'x' cannot be specified here")

  if(!is.null(custom))
    {
      cNames <- names(custom)
      reqNames <- c("parameters", "model", "prediction", "probability", "sort")
      if(!all(reqNames %in% cNames))
        stop(paste("The custom argument must be a list with elements", paste(reqNames, collapse = ", ")))
      if(!is.function(custom$model)) stop("The 'model' element should be a function")
      if(!is.function(custom$prediction)) stop("The 'prediction' element should be a function")
      if(!is.function(custom$probability) & !is.null(custom$probability)) stop("The 'probability' element should be a function or NULL")
      if(!is.function(custom$sort)) stop("The 'sort' element should be a function")
    }
  list(method = method,
       number = number,
       repeats = repeats,
       p = p,
       initialWindow = initialWindow,
       horizon = horizon,
       fixedWindow = fixedWindow,
       verboseIter = verboseIter,
       returnData = returnData,
       returnResamp = returnResamp,
       savePredictions = savePredictions,
       classProbs = classProbs,
       summaryFunction = summaryFunction,
       selectionFunction = selectionFunction,
       preProcOptions = preProcOptions,
       custom = custom,
       index = index,
       indexOut = indexOut,
       timingSamps = timingSamps,
       predictionBounds = predictionBounds,
       allowParallel = allowParallel)
}



