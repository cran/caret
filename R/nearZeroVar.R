nearZeroVarOld <- function(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE)
{
   if(is.vector(x)) x <- matrix(x, ncol = 1)
   freqRatio <- apply(
                      x, 
                      2, 
                      function(data)
                      {
                         dataTable <- sort(table(data[!is.na(data)]), decreasing = TRUE)
                         if(length(dataTable ) >= 2) 
                         {
                            dataTable [1]/dataTable[2]
                         } else 0
                      })
   percentUnique <- apply(
                          x, 
                          2, 
                          function(data) 100*length(unique(data[!is.na(data)]))/length(data))

   zeroVar <- apply(x, 2, function(data) length(unique(data[!is.na(data)])) == 1 | all(is.na(data)))
  
                          
   if(saveMetrics)
   {
      out <- data.frame(

         freqRatio = freqRatio, 
         percentUnique = percentUnique, 
         zeroVar = zeroVar,
         nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
   } else {
      out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
      names(out) <- NULL
   }
   out
 }

nearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, 
                         saveMetrics = FALSE)
{
  if (is.vector(x)) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data)
                     {
                       t <- table(data, useNA = "no")
                       if (length(t) <= 1) {
                         return(0);
                       }
                       w <- which.max(t);
                       return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
                     })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics)
    {
      out <- data.frame(freqRatio = freqRatio,
                        percentUnique = percentUnique,
                        zeroVar = zeroVar,
                        nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
  }
  out
}
