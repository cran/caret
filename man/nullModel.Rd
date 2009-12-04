\name{nullModel}
\alias{nullModel}
\alias{nullModel.default}
\alias{predict.nullModel}
\title{
Fit a simple, non-informative model
}
\description{
Fit a single mean or largest class model
}
\usage{
nullModel(x, ...)

\method{nullModel}{default}(x = NULL, y, ...)

\method{predict}{nullModel}(object, newdata = NULL, ...)
}
\arguments{
  \item{x}{
An optional matrix or data frame of predictors. These values are not used in the model fit
}
  \item{y}{
A numeric vector (for regression) or factor (for classification) of outcomes}
  \item{\dots}{
Optional arguments (not yet used)
}
  \item{object}{
An object of class \code{nullModel}
}
  \item{newdata}{
A matrix or data frame of predictors (only used to determine the number of predictions to return)
}
}
\details{
\code{nullModel} emulates other model building functions, but returns the simplest model possible given a training set: a single mean for numeric outcomes and the most prevalent class for factor outcomes.

}
\value{
The output of \code{nullModel} is a list of class \code{nullModel} with elements
  \item{call }{the function call}
  \item{value }{the mean of \code{y} or the most prevalent class}
  \item{levels }{when \code{y} is a factor, a vector of levels. \code{NULL} otherwise}
  \item{n }{the number of elements in \code{y}}

\code{predict.nullModel} returns a either a factor or numeric vector depending on the class of \code{y}. All predictions are always the same. }


\examples{
outcome <- factor(
                  sample(letters[1:2], 
                         size = 100, 
                         prob = c(.1, .9), 
                         replace = TRUE))
useless <- nullModel(y = outcome)
useless
predict(useless, matrix(NA, nrow = 10))

}
\keyword{models}

