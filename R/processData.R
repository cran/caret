"processData" <-
function(x, center = TRUE, scale = TRUE, na.remove = TRUE)
{
   numVar <- dim(x)[2]

   if(center) centerValue <- mean(x, na.rm = na.remove) else centerValue <- NA
   if(scale) scaleValue <- sd(x, na.rm = na.remove) else scaleValue <- NA

   out <- c(centerValue, scaleValue)
   
   names(out) <- c("mean", "std")
   out
}

