trainControl <- function(method = "boot",
                         number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
                         repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
                         verboseIter = TRUE,
                         returnData = TRUE,
                         returnResamp = "final",
                         p = .75,
                         classProbs = FALSE,
                         summaryFunction = defaultSummary,
                         selectionFunction = "best",
                         PCAthresh = 0.95,
                         ICAcomp = 3,
                         k = 5,
                         index = NULL,
                         workers = 1,
                         computeFunction = lapply,
                         computeArgs = NULL)
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  if(!(returnResamp %in% c("all", "final", "none"))) stop("incorrect value of returnResamp")
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
       PCAthresh = PCAthresh,
       ICAcomp = ICAcomp,
       k = k,
       index = index,
       workers = workers,
       computeFunction = computeFunction,
       computeArgs = computeArgs)
}



