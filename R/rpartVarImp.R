varImp.rpart <- function(object, ...) 
{
  outcomeName <- as.character(formula(object$terms)[[2]])
  dataTypes <- attributes(object$terms)$dataClasses
  isClassification <- if(dataTypes[names(dataTypes) == outcomeName] == "numeric") FALSE else TRUE

  frame <- object$frame[, 1:8]
  splitData <- object$split
  splitVar <- dimnames(splitData)[[1]]
  dimnames(splitData)[[1]] <- seq(along = splitVar)
  
  frame$node <- as.numeric(row.names(frame))
  frame$parent <- ifelse(frame$node == 1, 1, floor(frame$node/2))


  if(sum(frame$ncompete) + sum(frame$nsurrogate) > 0)
    {
      ## The start and end variables are denote the rows of object$split that
      ## can be used to figure out which splits were the primary, competing and
      ## surrogate splits.
      frame$end <- cumsum((frame$ncompete >0) + frame$ncompete + frame$nsurrogate)
      frame$start <- frame$end - (frame$ncompete + frame$nsurrogate)
      frame$end[frame$ncompete == 0] <- NA
      frame$start[frame$ncompete == 0] <- NA
      
      frame$improve <- splitData[frame$start, "improve"]
      frame <- frame[order(-frame$node, decreasing = TRUE),]
      
      
      splitInfo <- data.frame(
                              Node = rep(0, dim(splitData)[1]),
                              isCompeting = rep(FALSE, dim(splitData)[1]),   
                              isSurrogate = rep(FALSE, dim(splitData)[1]))
      
      noLeaves <- frame[frame$var != "<leaf>",]
      for(i in 1:dim(noLeaves)[1])
        {
          splitInfo$Node[noLeaves[i, "start"]:noLeaves[i, "end"]] <- noLeaves[i, "node"]
          if(noLeaves[i, "ncompete"] > 0)
            {
              splitInfo$isCompeting[(noLeaves[i, "start"] + 1):(noLeaves[i, "start"] + noLeaves[i, "ncompete"])] <- TRUE
            }
          if(noLeaves[i, "nsurrogate"] > 0)
            {
              splitInfo$isSurrogate[(noLeaves[i, "end"] - noLeaves[i, "nsurrogate"] + 1):noLeaves[i, "end"]] <- TRUE
            }     
        }   
      
      splitData <- cbind(data.frame(splitData), splitInfo)
      splitData$splitVar <- splitVar   
      rpartImp <- tapply(splitData$improve[!splitData$isSurrogate], splitData$splitVar[!splitData$isSurrogate], sum)

      impDF <- merge(data.frame(Feature = attributes(object$terms)$term.labels),
                     data.frame(Feature = names(rpartImp), importance = rpartImp), all = TRUE)
      impDF$importance[is.na(impDF$importance)] <- 0
    } else {
      ## In this case, things are simple since every row in object$split is
      ## a primary split
      splitData <- as.data.frame(object$split,
                                 row.names = paste(1:nrow((object$split))))                        
      splitData$Feature <- rownames(object$split)
      impVals <- aggregate(splitData$improve,
                           list(Feature = splitData$Feature),
                           sum)
      names(impVals)[2] <- "importance"
      impDF <- merge(impVals,
                     data.frame(Feature =  attributes(object$terms)$term.labels))
    }
  out <- data.frame(Overall = impDF$importance)
  rownames(out) <- impDF$Feature
  out
}

