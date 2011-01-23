\name{BoxCoxTrans.default}
\alias{BoxCoxTrans.default}
\alias{BoxCoxTrans}
\alias{predict.BoxCoxTrans}

\title{
Box-Cox Transformations
}
\description{
This class can be used to estimate a Box-Cox transformation and apply it to existing and future data
}
\usage{
BoxCoxTrans(y, ...)

\method{BoxCoxTrans}{default}(y, x = rep(1, length(y)), fudge = 0.2, numUnique = 3, na.rm = FALSE, ...)

\method{predict}{BoxCoxTrans}(object, newdata, ...)
}
\arguments{
  \item{y}{a strictly positive numeric vector of data to be transformed
}
  \item{x}{
an optional dependent variable to be used in a liner model
}
  \item{fudge}{
a tolerance value: lambda values within +/-fudge will be coerced to 0 and within 1+/-fudge will be coerced to 1
}
  \item{numUnique}{
how many unique values should \code{y} have to estimate the transformation?
}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped from \code{y} and \code{x} before the computation proceeds.}

  \item{\dots}{
for \code{BoxCoxTrans}: options ot pass to \code{\link[MASS]{boxcox}}. \code{plotit} should not be passed through. For \code{predict.BoxCoxTrans}, additional arguments are ignored.
}
  \item{object}{
an object of class \code{BoxCoxTrans}
}
  \item{newdata}{
a numeric vector of values to transform
}
}
\details{
This function is basically a wrapper for the \code{\link[MASS]{boxcox}} function in the MASS library. It can be used to estimate the transformation and apply it to new data. 

If \code{any(y <= 0)} or  if \code{length(unique(y)) < numUnique}, lambda is not estimated and no transformation is applied.

}
\value{
\code{BoxCoxTrans} returns a list of class \code{BoxCoxTrans} with elements
  \item{lambda }{estimated transformation value}
  \item{fudge }{value of \code{fudge}}
  \item{n }{number of data points used to estimate lambda}
  \item{summary }{the results of \code{summary(y)}}
  \item{ratio }{\code{max(y)/min(y)}}
  \item{skewness }{sample skewness statistic}

\code{predict.BoxCoxTrans} returns a numeric vector of transformed values
}
\references{
Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211-252.
}

\author{
Max Kuhn
}

\seealso{
\code{\link[MASS]{boxcox}}, \code{\link{preProcess}}
}
\examples{
data(BloodBrain)

ratio <- exp(logBBB)
bc <- BoxCoxTrans(ratio)

bc

predict(bc, ratio[1:5])


ratio[5] <- NA
bc2 <- BoxCoxTrans(ratio, bbbDescr$tpsa, na.rm = TRUE)
bc2


}
\keyword{ utilities }
