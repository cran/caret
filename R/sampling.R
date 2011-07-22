


downSample <- function(x, y)
  {
    xc <- class(x)
    if(!is.data.frame(x)) x <- as.data.frame(x)
    if(!is.factor(y))
      {
        warning("Down-sampling requires a factor variable as the response. The original data was returned.")
        return(list(x = x, y = y))
      }

    minClass <- min(table(y))
    x$.outcome <- y
    
    x <- ddply(x, .(y),
               function(dat, n) dat[sample(seq(along = dat$.outcome), n),,drop = FALSE],
               n = minClass)
    y <- x$.outcome
    x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
    if(xc[1] == "matrix") x <- as.matrix(x)
    list(x = x, y = y)
  }


if(FALSE)
  {
    x <- matrix(1:90, ncol = 3)
    y <- factor(sample(letters[1:3], nrow(x), prob = c(.1, .45, .45),
                       replace = TRUE))
    table(y)
    downSample(x, y)
  }
