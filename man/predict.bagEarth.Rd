\name{predict.bagEarth}
\alias{predict.bagEarth}
\alias{predict.bagFDA}
\title{Predicted values based on bagged Earth and FDA models}
\description{
Predicted values based on bagged Earth and FDA models
}
\usage{
predict.bagEarth(object, newdata = NULL, ...)
predict.bagFDA(object, newdata = NULL, type = "class", ...)
}
\arguments{
  \item{object}{Object of class inheriting from \code{bagEarth}}
  \item{newdata}{ An optional data frame or matrix in which to look for variables with
          which to predict.  If omitted, the fitted values are used (see note below).}
  \item{type}{\code{bagFDA} only: if \code{type = "class"} is specified, a factor of class predictions (with the same levels as the original training outcome) are returned. If \code{type = "probs"}, a matrix of class probabilities (i.e. vote rates) is returned.}          
  \item{\dots}{not used}
}

\value{
a vector of predictions 
}
\author{Max Kuhn}
\note{
If the predictions for the original training set are needed, there are two ways to calculate them. First, the original data set can be predicted by each bagged earth model. Secondly, the predictions form each bootstrap sample could be used (but are more likely to overfit). If the original call to \code{bagEarth} or \code{bagFDA} had \code{keepX = TRUE}, the first method is used, otherwise the values are calculated via the second method.
}
\seealso{\code{\link{bagEarth}}}
\examples{
data(trees)
fit1 <- bagEarth(Volume ~ ., data = trees, keepX = TRUE)
fit2 <- bagEarth(Volume ~ ., data = trees, keepX = FALSE)
hist(predict(fit1) - predict(fit2))

}
\keyword{regression}


