"varImp.pamrtrained" <-
function (object, threshold, data, ...) 
{
   if( dim(object$centroids)[1] != dim(data)[2]) 
      stop("the number of columns (=variables) is not consistent with the pamr object")
      
   if(is.null(dimnames(data))) 
   {
      featureNames <- paste("Feature", seq(along = data[1,]), sep = "")
      colnames(data) <- featureNames
   } else featureNames <- dimnames(data)[[2]]
   
   x <- t(data)
   retainedX <- x[object$gene.subset, object$sample.subset, drop = F]
   centroids <- pamr.predict(object, x, threshold = threshold, type = "cent")
   standCentroids <- (centroids - object$centroid.overall)/object$sd
   rownames(standCentroids) <- featureNames
   colnames(standCentroids) <- names(object$prior)
  
   as.data.frame(standCentroids)
}

