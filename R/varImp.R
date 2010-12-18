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

varImp.multinom <- function(object, ...)
  {
    out <- abs(coef(object))
    if(is.vector(out))
      {
        out <- data.frame(Overall = out)
        rownames(out) <- names(coef(object))
      } else {
        out <- as.data.frame(apply(out, 2, sum))
        names(out)[1] <- "Overall"
      }
    subset(out, rownames(out) != "(Intercept)")
  }


varImp.gam <- function(object, ...)
  {

    if(any(names(object) %in% c("edf", "mgcv.conv", "gcv.ubre")))
      {
        library(mgcv)
        tmp <- mgcv:::anova.gam(object)
        smoothed <- data.frame(Overall = tmp$s.table[, 4])

        if(nrow(tmp$p.table) > 1)
          {
            linear <- data.frame(Overall = tmp$p.table[, 4])
            out <- rbind(linear, smoothed)
          } else out <- smoothed
        out$Overall[!is.na(out$Overall)] <- -log10(out$Overall[!is.na(out$Overall)])

        out$Overall[is.na(out$Overall)] <- 0

        nms <- strsplit(rownames(out), "[()]")
        nms <- unlist(lapply(nms, function(x) x[length(x)]))
        rownames(out) <- nms
        out <- subset(out, rownames(out) != "Intercept")
      } else {
        library(gam)
        trms <- attr(object$terms, "term.labels")

        vars <- all.vars(object$terms)[-1]
        out <- data.frame(Overall = rep(NA, length(vars)))
        rownames(out) <- vars
        for(i in seq(along = trms))
          {
            reduced <- update(object, as.formula(paste("~.-", trms[i])))
            out[i,1] <- -log10(gam:::anova.gam(object, reduced)[2, "P(>|Chi|)"])
          }
      }
    out
  }
