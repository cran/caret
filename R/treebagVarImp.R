varImp.regbagg <- varImp.classbagg <- function(object, ...)
{
   applyRpart <- lapply(
      object$mtrees, 
      function(bagObject, varNames)
      {
         bagObject$btree$xNames <- varNames
         out <- varImp(bagObject$btree)
         out
       },
       varNames = object$xNames)
       
   summedImp <- as.data.frame(apply(data.frame(applyRpart), 1, sum))
   names(summedImp) <- "Overall"
   summedImp
}

