\name{postResample}
\alias{postResample}
\alias{defaultSummary}
\title{Calculates performance across resamples}
\description{
Given two numeric vectors of data, the mean squared error and R-squared
are calculated. For two factors, the overall agreement rate and Kappa are 
determined.
}
\usage{
postResample(pred, obs)
defaultSummary(data, lev = NULL, model = NULL)
}
\arguments{
  \item{pred}{A vector of numeric data (could be a factor)}
  \item{obs}{A vector of numeric data (could be a factor)}
  \item{data}{a data frame or matrix with columns \code{obs} and \code{pred} for hte observed and predicted outcomes}
  \item{lev}{a character vector of factors levels for the response. In regression cases, this would be \code{NULL}.}
  \item{model}{a character string for the model name (as taken form the \code{method} argument of \code{\link{train}}.}
}
\details{
\code{postResample} is meant to be used with \code{apply} across a matrix. For numeric data
the code checks to see if the standard deviation of either vector is zero. If so, the correlation
between those samples is assigned a value of zero. \code{NA} values are ignored everywhere.

Note that many models have more predictors (or parameters) than data points, so the typical mean squared
error denominator (n - p) does not apply. Root mean squared error is calculated using \code{sqrt(mean((pred - obs)^2}. 
Also, R-squared is calculated as the square of the correlation between the observed and predicted outcomes. 

For \code{defaultSummary} is the default function to compute performance metrics in \code{\link{train}}. It is a wrapper around \code{postResample}. 

Other functions can be used via the \code{summaryFunction} argument of \code{\link{trainControl}}. Custom functions must have the same arguments as\code{defaultSummary}.

}
\value{
   A vector of performance estimates.
}
\author{Max Kuhn}

\seealso{\code{\link{trainControl}}}
\examples{
predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)
}
\keyword{utilities}


