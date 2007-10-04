maxDissim <- function(a, b, n = 2, obj = minDiss, randomFrac = 1, verbose = FALSE, ...) 
{
   library(proxy)
	if(nrow(b) < 2) stop("there must be at least 2 samples in b")
	if(ncol(a) != ncol(b)) stop("a and b must have the same number of columns")
	if(nrow(b) < n) stop("n must be less than nrow(b)")
	if(randomFrac > 1 | randomFrac <= 0) stop("randomFrac must be in (0, 1]")
	
	inSubset <- NULL
	free <- 1:nrow(b)
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
   	diss <- dist(newA, b[pool,, drop = FALSE], ...)	
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

