\name{postResample}
\alias{postResample}
\title{Calculates performance across resamples}
\description{
Given two numeric vectors of data, the mean squared error and R-squared
are calculated. For two factors, the overall agreement rate and Kappa are 
determined.
}
\usage{
postResample(pred, obs)
}
\arguments{
  \item{pred}{A vector of numeric data (could be a factor)}
  \item{obs}{A vector of numeric data (could be a factor)}
}
\details{
This function is meant to be used with \code{apply} across a matrix. For numeric data
the code checks to see if the standard deviation of either vector is zero. If so, the correlation
between those samples is assigned a value of zero. \code{NA} values are ignored everywhere.

Note that many models have more predictors (or parameters) than data points, so the typical mean squared
error denominator (n - p) does not apply. Root mean squared error is calculated using \code{sqrt(mean((pred - obs)^2}. 
Also, R-squared is calculated as the square of the correlation between the observed and predicted outcomes. 
}
\value{
   A vector of performance estimates.
}
\author{Max Kuhn}

\seealso{\code{\link{resampleSummary}}}
\examples{
predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)
}
\keyword{utilities}


