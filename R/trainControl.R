trainControl <- function(
   method = "boot",
   number = ifelse(method == "cv", 10, 25),
   verboseIter = TRUE,
   returnData = TRUE,
   p = .5,
   index = NULL)
{
   list(
      method = method,
      number = number,
      verboseIter = verboseIter,
      returnData = returnData,
      p = p,
      index = index)
}

