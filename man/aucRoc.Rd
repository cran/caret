\name{aucRoc}
\alias{aucRoc}
\title{Compute the area under an ROC curve}
\description{
This function uses the trapezoidal rule to calculate the area. If the area is less than .5, then \code{1 - area} is used.
}
\usage{
aucRoc(object)
}
\arguments{
  \item{object}{object from the \code{roc} function or a matrix/data
    frame with columns named "sensitivity" and "specificity"}
}

\value{
a scalar number
}
\author{Max Kuhn}

\seealso{\code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{roc}}}

\examples{
set.seed(6)
testData <- data.frame(
   x = c(rnorm(200), rnorm(200) + 1),
   group = factor(rep(letters[1:2], each = 200)))
   
densityplot(~testData$x, groups = testData$group, auto.key = TRUE)   
   
rocValues <- roc(testData$x, testData$group)
aucRoc(rocValues)
}
\keyword{manip}

