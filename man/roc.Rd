\name{roc}
\alias{roc}
\title{Compute the points for an ROC curve}
\description{
  Computes sensitivity and specificity for a variety of cutoffs
}
\usage{
roc(data, class, dataGrid = TRUE, gridLength = 100, positive = levels(class)[1])
}

\arguments{
  \item{data}{a numeric variable to cut along}
  \item{class}{a factor with class memberships. There must be only two classes.}
  \item{dataGrid}{should the data define the grid of cut-points? If not a sequence of evenly spaced intervals is used.}
  \item{gridLength}{number of intervals to use if the data do not define the grid.}
  \item{positive}{a character string for the level of the class variable that defines a "positive" event}
}

\value{
   A matrix of results with columns "cutoff", "sensitivity" and "specificity"
}
\author{Max Kuhn}
\note{The first row in the output has a cutoff of \code{NA}, zero sensitivity and specificity of one.}

\seealso{\code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{aucRoc}}}
\examples{
set.seed(6)
testData <- data.frame(
   x = c(rnorm(200), rnorm(200) + 1),
   group = factor(rep(letters[1:2], each = 200)))
   
densityplot(~testData$x, groups = testData$group, auto.key = TRUE)   
   
roc(testData$x, testData$group)
}
\keyword{manip}
