format.bagEarth <- function(x, ...) 
{
   library(earth)
   cat("(\n") 
	 cat(format(x$fit[[1]], ...))
	 if(length(x$fit) > 1) lapply(x$fit[-1], function(u, ...) cat("  +\n", format(u, ...)), ...)
	 cat(")/", x$B, "\n", sep = "")
}

