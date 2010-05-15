"varImp" <-
function(object, ...){
   UseMethod("varImp")
}



varImp.dsa <- function(object, cuts = NULL, ...)
  {
    if(is.null(cuts) & !is.null(object$tuneValue))
      {
        cuts <- object$tuneValue$.cut.off.growth[1]
      } else {
        if(is.null(cuts)) stop("please supply a value for 'cuts'")
      }
    tmp <- object$var.importance[,cuts]
    out <- data.frame(Overall = tmp)
    rownames(out) <- names(tmp)
    out



  }
