trainControl <- function(method = "boot",
                         number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
                         repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
                         verboseIter = FALSE,
                         returnData = TRUE,
                         returnResamp = "final",
                         p = .75,
                         classProbs = FALSE,
                         summaryFunction = defaultSummary,
                         selectionFunction = "best",
                         preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
                         index = NULL,
                         timingSamps = 0,
                         predictionBounds = rep(FALSE, 2))
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  if(!(returnResamp %in% c("all", "final", "none"))) stop("incorrect value of returnResamp")
  if(length(predictionBounds) > 0 && length(predictionBounds) != 2) stop("'predictionBounds' should be a logical or numeric vector of length 2")
  if(any(names(preProcOptions) == "method")) stop("'method' cannot be specified here")
  if(any(names(preProcOptions) == "x")) stop("'x' cannot be specified here")
  
  list(method = method,
       number = number,
       repeats = repeats,
       verboseIter = verboseIter,
       returnData = returnData,
       returnResamp = returnResamp,
       p = p,
       classProbs = classProbs,
       summaryFunction = summaryFunction,
       selectionFunction = selectionFunction,
       preProcOptions = preProcOptions,
       index = index,
       timingSamps = timingSamps,
       predictionBounds = predictionBounds)
}



