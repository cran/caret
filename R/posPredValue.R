"posPredValue" <-
function(data, reference, positive = levels(reference)[1])
{
   if(!is.factor(reference) | !is.factor(data)) 
      stop("inputs must be factors")
      
   if(length(unique(c(levels(reference), levels(data)))) != 2)
      stop("input data must have the same two levels")
   
   numer <- sum(data == positive & reference == positive)
   denom <- sum(data == positive)
   posPred <- ifelse(denom > 0, numer / denom, NA)
   posPred
}

