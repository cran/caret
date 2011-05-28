varImp.RandomForest <- function(object, ...)
{
   library(party)
   variableImp <- varimp(object, ...)
   out <- data.frame(Overall = variableImp)
   out
}

