
varImp.earth <- function(object, value = "gcv", ...)
{

  library(earth)
  
  earthImp <- evimp(object)
  if(!is.matrix(earthImp)) earthImp <- t(as.matrix(earthImp))


  # get other variable names and padd with zeros
  
  out <- earthImp
  perfCol <- which(colnames(out) == value)

  increaseInd <- out[,perfCol + 1]
  out <- as.data.frame(out[,perfCol, drop = FALSE])  
  colnames(out) <- "Overall"

  
  # At this point, we still may have some variables
  # that are not in the model but have non-zero
  # importance. We'll set those to zero
  if(any(earthImp[,"used"] == 0))
    {
      dropList <- grep("-unused", rownames(earthImp), value = TRUE)
      out$Overall[rownames(out) %in% dropList] <- 0
    }
  
  rownames(out) <- gsub("-unused", "", rownames(out))

  out <- as.data.frame(out)


  # fill in zeros for any variabels not  in out

  xNames <- object$namesx.org
  if(any(!(xNames %in% rownames(out))))
    {
      xNames <- xNames[!(xNames %in% rownames(out))]
      others <- data.frame(
                           Overall = rep(0, length(xNames)),
                           row.names = xNames)
      out <- rbind(out, others)
    }
  out
  
}

