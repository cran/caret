
"specificity" <-
function(data, reference, negative = levels(reference)[2])
{
   if(!is.factor(reference) | !is.factor(data)) 
      stop("input data must be a factor")
   
   if(length(unique(c(levels(reference), levels(data)))) != 2)
      stop("input data must have the same two levels")   
   
   numer <- sum(data == negative & reference == negative)
   denom <- sum(reference == negative)
   spec <- ifelse(denom > 0, numer / denom, NA)  
   spec
}

