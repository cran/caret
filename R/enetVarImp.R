varImp.enet <- function(object, s = NULL, ...)
  {

    library(elasticnet)

    if(is.null(s))
      {
        if(!is.null(object$tuneValue$.fraction))
          {
            s <- object$tuneValue$.fraction
          } else stop("you must specify s as a fraction")
      }
    

    beta <- predict(
                    object,
                    s = s,
                    type = "coefficients",
                    mode = "fraction")$coefficients
    out <- data.frame(Overall = abs(beta))
    rownames(out) <- names(beta)
    out
    
  }
