postResample <- function(pred, obs)
{

   isNA <- is.na(pred)
   pred <- pred[!isNA]
   obs <- obs[!isNA]

   if(!is.factor(obs) & is.numeric(obs))
   {
      if(sd(pred) == 0 || sd(obs) == 0) resamplCor <- NA
         else resamplCor <- cor(pred, obs, use = "pairwise.complete.obs")
      mse <- mean((pred - obs)^2)
      n <- length(obs)

      out <- c(sqrt(mse), resamplCor^2)
      names(out) <- c("RMSE", "Rsquared")    
   } else {
     pred <- factor(pred, levels = levels(obs))  
     library(e1071)   
      out <- unlist(classAgreement(table(obs, pred)))[c("diag", "kappa")]
      names(out) <- c("Accuracy", "Kappa")         
   }
   out
}
