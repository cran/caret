preProcess <- function(x, ...)
  UseMethod("preProcess")

preProcess.default <- function(x, method = c("center", "scale"), thresh = 0.95, na.remove = TRUE, ...)
{
   theCall <- match.call(expand.dots = TRUE)
   if(any(method == "center")) centerValue <- apply(x, 2, mean, na.rm = na.remove) else centerValue <- NA
   if(any(method %in% c("scale", "pca"))) scaleValue <- apply(x, 2, sd, na.rm = na.remove) else scaleValue <- NA

   if(any(scaleValue == 0))
   {
      stop(
           paste(
                 "These variables have zero variances:",
                 paste(
                       names(scaleValue)[which(scaleValue == 0)],
                       collapse = ", ")))
   }
   
   if(any(method == "pca"))
   {
      tmp <- prcomp(x, scale = TRUE, retx = FALSE)
      cumVar <- cumsum(tmp$sdev^2/sum(tmp$sdev^2)) 
      numComp <- max(2, which.max(cumVar > thresh))
      rot <- tmp$rotation[,1:numComp]
    } else {
      rot <- NULL
      numComp <- NULL
    }
   
   out <- list(
               call = theCall,
               dim = dim(x),
               mean = centerValue,
               std = scaleValue,
               rotation = rot,
               method = method,
               thresh = thresh,
               numComp = numComp)
   structure(out, class = "preProcess")
   
}

predict.preProcess <- function(object, newdata, ...)
{

   dataNames <- colnames(newdata)
   x <- newdata
   if(any(object$method == "center")) x <- sweep(x, 2, object$mean, "-")
   if(any(object$method %in% c("scale", "pca"))) x <- sweep(x, 2, object$std, "/")
   if(any(object$method == "pca"))
   {
      x <-if(is.matrix(x)) x %*% object$rotation else as.matrix(x) %*% object$rotation
      if(any(class(newdata) == "data.frame")) x <- as.data.frame(x)
    }

   if(any(object$method == "spatialsign")) x <- spatialSign(x)
   if(all(object$method != "pca")) colnames(x) <- dataNames
   x
}

print.preProcess <- function(x, ...)
{
   cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
   cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n")
   if(any(x$method == "pca"))
   {
      cat("PCA needed", x$numComp, "components to capture", round(x$thresh*100, 2),
          "percent of the variance\n")

   } 
}
