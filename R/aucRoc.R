aucRoc <- function(object)
{
   sens <- object[, "sensitivity"]
   omspec <- 1 - object[, "specificity"]
   newOrder <- order(omspec)
   sens <- sens[newOrder]
   omspec <- omspec[newOrder]
   
   rocArea <- sum(.5 *diff(omspec) * (sens[-1] + sens[-length(sens)]))
   rocArea <- max(rocArea, 1 - rocArea)
   rocArea
}

