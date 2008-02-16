\name{applyProcessing}
\alias{applyProcessing}
\alias{processData}
\title{Data Processing on Predictor Variables (Deprecated) }
\description{
  \code{processData} is used to estimate data processing parameters and
  \code{applyProcessing} takes the results  and applies them to data objects
}
\usage{
processData(x, center = TRUE, scale = TRUE, na.remove = TRUE)
applyProcessing(x, object)
}
\arguments{
  \item{x}{A matrix or data frame with variables in rows and samples in columns}  
  \item{center}{A boolean to center the data.}
  \item{scale}{A boolean to scale the data.}
  \item{na.remove}{Should \code{NA} values be removed?}
  \item{object}{the results from a previous call to \link{processData}.}
}

\details{
These functions are deprecated and have been replaced by the \code{\link{preProcess}} class. In a few more versions, they will not be around.
}


\value{
\code{x}, after it has been processed.
}

\author{Max Kuhn}

\seealso{\code{\link{applyProcessing}} }
\examples{
set.seed(115)
xData <- matrix(rgamma(6 * 4, 5), 6)

trainValues01 <- apply(xData, 2, processData)
applyProcessing(xData, trainValues01)

}
\keyword{manip}
