maxDissim <- function(a, b, n = 2, obj = minDiss, useNames = FALSE, randomFrac = 1, verbose = FALSE, ...) 
{
  library(proxy)
  if(nrow(b) < 2) stop("there must be at least 2 samples in b")
  if(ncol(a) != ncol(b)) stop("a and b must have the same number of columns")
  if(nrow(b) < n) stop("n must be less than nrow(b)")
  if(randomFrac > 1 | randomFrac <= 0) stop("randomFrac must be in (0, 1]")


  if(useNames)
    {
      if(is.null(rownames(b)))
        {
          warning("Cannot use rownames; swithcing to indices")
          free <- 1:nrow(b)
        } else free <- rownames(b)
    } else free <- 1:nrow(b)

  inSubset <- NULL
  newA <- a
  
  
  if(verbose) cat("  adding:")
  for(i in 1:n)
    {
      pool <- if(randomFrac == 1) free else sample(free, max(2, floor(randomFrac * length(free))))
      if(verbose)
        {
          cat("\nIter", i, "\n")
          cat("Number of candidates:", length(free), "\n")
          cat("Sampling from", length(pool), "samples\n")		
        }
      diss <- proxy:::dist(newA, b[pool,, drop = FALSE], ...)
      bNames <- colnames(b)[pool] 
      tmp <- pool[which.max(apply(diss, 2, obj))]
      if(verbose)cat("new sample:", tmp, "\n")      
      inSubset <- c(inSubset, tmp)
      newA <- rbind(newA, b[tmp,, drop = FALSE])
      free <- free[!(free %in% inSubset)]
    }
  inSubset
}

minDiss <- function(u) min(u, na.rm = TRUE)

sumDiss <- function(u) sum(u, na.rm = TRUE)







splitter <- function(x, p = .8, start = NULL, ...)
  {
    n <- nrow(x)
    if(is.null(start)) start <- sample(1:n, 1)
    n2 <- n - length(start)
    m <- ceiling(p * n2)
    pool <- maxDissim(x[ start,,drop = FALSE],
                      x[-start,,drop = FALSE],
                      n = m,
                      ...)
    c(start, pool)
  }


splitByDissim <- function(x, p = .8, y = NULL, start = NULL, ...)
  {
    library(plyr)
    if(!is.data.frame(x)) x <- as.data.frame(x)
    
    if(!is.null(y))
      {
        if(!is.factor(y)) y <- as.factor(y)
        lvl <- levels(y)
        
        ind <- split(seq(along = y), y)
        ind2 <- lapply(ind, function(x) seq(along = x))
        start2 <- lapply(ind, function(x, start) which(x %in% start),
                         start = start)
        for(i in seq(along = lvl))
          {
            tmp <- splitter(x[ind[[i]],, drop = FALSE],
                            p = p,
                            start = start2[[i]],
                            ...)
            tmp2 <- ind[[i]][which(ind2[[i]] %in% tmp)]
            out <- if(i == 1) tmp2 else c(tmp2, out)
          }
      } else {
        out <- splitter(x, p = p, start = start, ...)
      }
    out
  }



if(FALSE)
  {
    ind <- splitByDissim(iris[, 1:2],  p = .1, y = iris$Species, start = c(1:2, 51:52, 101:102))
    iris$Group <- "train"
    iris$Group[ind] <- "test"
    xyplot(Sepal.Length ~ Sepal.Width|Species, data = iris, groups = Group)

    plotTheme <- caretTheme()
    plotTheme$superpose.symbol$col[1] <- "lightgrey"
    trellis.par.set(plotTheme)

    data(mdrr)
    set.seed(100)
    strt <- createDataPartition(mdrrClass, p = .05)[[1]]
    ind <- splitByDissim(mdrrDescr[, 1:2],  p = .15, y = mdrrClass, start = strt)
    mdrrDescr$Group <- "pool"
    mdrrDescr$Group[ind[!(ind %in% strt)]] <- "selected"
    mdrrDescr$Group[strt] <- "start"
    
    xyplot(MW ~ AMW|mdrrClass, data = mdrrDescr, groups = Group, auto.key = list(columns = 3))

  }
