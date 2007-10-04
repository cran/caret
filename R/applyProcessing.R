"applyProcessing" <-
function(x, object)
{
   dataNames <- dimnames(object)[[2]]
   if(!all(is.na(object[1,]))) x <- sweep(x, 2, object[1,], "-")
   if(!all(is.na(object[2,]))) x <- sweep(x, 2, object[2,], "/")
   dimnames(x)[[2]] <- dataNames
   x
}

