varImp.rpart <- function(object, surrogates = FALSE, competes = TRUE, ...)
  {
    tmp <- rownames(object$splits)
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""

    frame <- as.data.frame(object$frame)
    index <- 0
    for(i in 1:nrow(frame))
      {
        if(frame$var[i] != "<leaf>")
          {
            index <- index + 1
            splits$type[index] <- "primary"
            if(frame$ncompete[i] > 0)
              {
                for(j in 1:frame$ncompete[i])
                  {
                    index <- index + 1
                    splits$type[index] <- "competing"
                  }
              }
            if(frame$nsurrogate[i] > 0)
              {
                for(j in 1:frame$nsurrogate[i])
                  {
                    index <- index + 1
                    splits$type[index] <- "surrogate"
                  }
              }
          }
      }
    splits$var <- factor(as.character(splits$var))
    if(!surrogates) splits <- subset(splits, type != "surrogate")
    if(!competes) splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve,
                     list(Variable = splits$var),
                     sum,
                     na.rm = TRUE)

    allVars <- colnames(attributes(object$terms)$factors)
    if(!all(allVars %in% out$Variable))
      {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missingVars)),
                            Variable = missingVars)
        out <- rbind(out, zeros)
      }

    out2 <- data.frame(Overall = out$x)
    rownames(out2) <- out$Variable
    out2

  }
