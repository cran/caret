format.bagEarth <- function(x, file = "", ...) 
{
  library(earth)

  eachEq <- lapply(
                   x$fit,
                   function(u, ...) format(u, ...),
                   ...)
  allEq <- paste(
                 "(",
                 paste(eachEq, collapse = "+"),
                 ") /",
                 x$B,
                 "\n")
  
  cat(allEq, file = file)
}

