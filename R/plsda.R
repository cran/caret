plsda <- function (x, ...)
   UseMethod("plsda")



predict.plsda <- function(object, newdata = NULL, ncomp = NULL, type = "class", ...)
{
   library(pls)
   if(is.null(ncomp))
   {
      if(!is.null(object$ncomp)) ncomp <- object$ncomp else stop("specify ncomp")
   }
   
   tmpPred <- predict.mvr(object, newdata = newdata)[,,ncomp,drop = FALSE]
   
   switch(type,
      raw =
      {
         out <- tmpPred
      },
      class =
      {
         if(length(dim(tmpPred)) < 3)
         {
            #only requested one component
            out <- object$obsLevels[apply(tmpPred, 1, which.max)]
            out <- factor(out, levels = object$obsLevels)
         } else {
            # more than one component
            tmpOut <- matrix("", nrow = dim(tmpPred)[1], ncol = dim(tmpPred)[3])
            for(i in 1:dim(tmpPred)[3])
            {
               tmpOut[,i] <- object$obsLevels[apply(tmpPred[,,i,drop=FALSE], 1, which.max)]
            }
            out <- as.data.frame(tmpOut)
            out <- as.data.frame(lapply(out, function(x, y) factor(x, levels = y), y = object$obsLevels))
            names(out) <- paste("ncomp", ncomp, sep = "")
            if(length(ncomp) == 1) out <- out[,1]
         }
      },
      prob =
      {
         if(length(dim(tmpPred)) < 3)
         {
            out <- t(apply(tmpPred, 1, function(data) exp(data)/sum(exp(data))))
         } else {
            # more than one component
            out <- tmpPred * NA
            for(i in 1:dim(tmpPred)[3])
            {
               out[,,i] <- t(apply(tmpPred[,,i,drop=FALSE], 1, function(data) exp(data)/sum(exp(data))))
            }
         }
      }
   )
   out
}



plsda.default <- function(x, y, ncomp = 2, ...)
{
   library(pls)

   funcCall <- match.call(expand.dots = TRUE)
     
   # from nnet.formula
   class.ind <- function(cl) {
      n <- length(cl)
      x <- matrix(0, n, length(levels(cl)))
      x[(1:n) + n * (as.vector(unclass(cl)) - 1)] <- 1
      dimnames(x) <- list(names(cl), levels(cl))
      x
   }

   if(is.factor(y))
   {
      obsLevels <- levels(y)
      y <- class.ind(y)
   } else {
      if(is.matrix(y))
      {
         test <- apply(y, 1, sum)
         if(any(test != 1)) stop("the rows of y must be 0/1 and sum to 1")
         obsLevels <- colnames(y)
         if(is.null(obsLevels)) stop("the y matrix must have column names")
      } else stop("y must be a matrix or a factor")
   }
   
   if(!is.matrix(x)) x <- as.matrix(x)

   tmpData <- data.frame(n = paste("row", 1:nrow(y), sep = ""))
   tmpData$y <- y
   tmpData$x <- x

   out <- plsr(
      y ~ x,
      data = tmpData,
      ncomp = ncomp,
      ...)
      
   out$obsLevels <- obsLevels
   #out$call <- funcCall
   class(out) <- c("plsda", class(out))
   out
}

print.plsda <- function (x, ...)
{
   # minor change to print.mvr
    switch(x$method, kernelpls = {
        regr = "Partial least squares"
        alg = "kernel"
    }, simpls = {
        regr = "Partial least squares"
        alg = "simpls"
    }, oscorespls = {
        regr = "Partial least squares"
        alg = "orthogonal scores"
    }, svdpc = {
        regr = "Principal component"
        alg = "singular value decomposition"
    }, stop("Unknown fit method."))
    cat(regr, "classification, fitted with the", alg, "algorithm.")
    if (!is.null(x$validation))
        cat("\nCross-validated using", length(x$validation$segments),
            attr(x$validation$segments, "type"), "segments.")
    cat("\nCall:\n", deparse(x$call), "\n", sep = "")
    invisible(x)
}
