\name{nearZeroVar}
\alias{nearZeroVar}

\title{Identification of near zero variance predictors}
\description{
This function diagnoses predictors that have one unique value (i.e. are zero variance predictors) or predictors that are have both of the following characteristics: they have very few unique values relative to the number of samples and the ratio of the frequency of the most common value to the frequency of the second most common value is large. }
\usage{
nearZeroVar(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE)
}
\arguments{
  \item{x}{a numeric vector or matrix, or a data frame with all numeric data}
  \item{freqCut}{the cutoff for the ratio of the most common value to the second most common value }
  \item{uniqueCut}{ the cutoff for the percentage of distinct values out of the number of total samples}
  \item{saveMetrics}{a logical. If false, the positions of the zero- or near-zero predictors is returned. If true, a data frame with predictor information is returned. }
}
\details{
For example, an example of near zero variance predictor is one that, for
1000 samples, has two distinct values and 999 of them are a single
value.

To be flagged, first the frequency of the most prevalent value over the
second most frequent value (called the ``frequency ratio'') must be
above \code{freqCut}. Secondly, the ``percent of unique values,''  the
number of unique values divided by the total number of samples (times
100), must also be below \code{uniqueCut}.

In the above example, the frequency ratio is 999 and the unique value
percentage is 0.0001.

}
\value{
If \code{saveMetrics = FALSE}, a vector of integers corresponding to the column positions of the problematic predictors. If \code{saveMetrics = TRUE}, a data frame with columns:
  \item{freqRatio }{the ratio of frequencies for the most common value over the second most common value}
  \item{percentUnique }{the percentage of unique data points out of the total number of data points}
  \item{zeroVar }{a vector of logicals for whether the predictor has only one distinct value}
  \item{nzv }{a vector of logicals for whether the predictor is a near zero variance predictor}

}

\author{Max Kuhn, speed improvements by Allan Engelhardt}

\examples{
nearZeroVar(iris[, -5], saveMetrics = TRUE)

data(BloodBrain)
nearZeroVar(bbbDescr)


}
\keyword{utilities}

