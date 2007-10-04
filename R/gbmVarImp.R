varImp.gbm <- function(object, numTrees = object$n.trees, ...) 
{
   require(gbm)
   varImp <- relative.influence(object, n.trees = numTrees)
   out <- data.frame(varImp)
   colnames(out) <- "Overall"
   rownames(out) <- object$var.names
   out   
}

