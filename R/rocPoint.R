rocPoint <- function(cutoff, x, y, positive)
{
   classLevels <- levels(y)
   negative <- classLevels[positive != classLevels]
   newClass <- factor(
      ifelse(
         x <= cutoff, 
         negative,
         positive), 
         levels = classLevels)
   out <- c(
      cutoff,
      sensitivity(newClass, y), 
      specificity(newClass, y))
   names(out) <- c("cutoff", "sensitivity", "specificity")
   out
}

