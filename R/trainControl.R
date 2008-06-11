trainControl <- function(
                         method = "boot",
                         number = ifelse(method == "cv", 10, 25),
                         verboseIter = TRUE,
                         returnData = TRUE,
                         p = .5,
                         selectionFunction = "best",
                         index = NULL)
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  list(
       method = method,
       number = number,
       verboseIter = verboseIter,
       returnData = returnData,
       p = p,
       selectionFunction = selectionFunction,
       index = index)
}

