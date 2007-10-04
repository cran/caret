getTrainPerf <- function(x)
{
  tuneAcc <- x$results
  tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
  keepCols <- if(x$modelType == "Regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
  if (dim(tuneAcc)[1] > 1) 
  {
   optPerf <- if (x$metric != "RMSE") 
            which.max(tuneAcc[, x$metric])
        else which.min(tuneAcc[, x$metric])
      out <- tuneAcc[optPerf, keepCols]
   } else out <- tuneAcc[1, keepCols]
   names(out) <- paste("Train", names(out), sep = "")
   out$method <- x$call[["method"]]
   
   out
}

