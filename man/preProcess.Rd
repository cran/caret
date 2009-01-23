\name{preProcess}
\alias{preProcess}
\alias{preProcess.default}
\alias{predict.preProcess}

\title{Pre-Processing of Predictors }
\description{
Pre-processing transformation (centering, scaling etc) can be estimated from the training data and applied to any data set with the same variables. 
}
\usage{
preProcess(x, ...)

\method{preProcess}{default}(x, method = c("center", "scale"), 
           thresh = 0.95, na.remove = TRUE, ...)

\method{predict}{preProcess}(object, newdata, ...)
}

\arguments{
  \item{x}{a matrix or data frame}
  \item{method}{a character vector specifying the type of processing. Possible values are "center", "scale", "pca" and "spartialSign"}
  \item{thresh}{a cutoff for the cumulative percent of variance to be retained by PCA}
  \item{na.remove}{a logical; should missing values be removed from the calculations?}
  \item{object}{an object of class \code{preProcess}}
  \item{newdata}{a matrix or data frame of new data to be pre-processed}
  \item{\dots}{Additional arguments (currently this argument is not used)}
}
\details{
The operations are applied in this order: centering, scaling, PCA and spatial sign. If PCA is requested but scaling is not, the values will still be scaled. 

The function will throw an error of any variables in \code{x} has less than two unique values. 
}
\value{
\code{preProcess} results in a list with elements
  \item{call}{the function call}
  \item{dim}{the dimensions of \code{x}}
  \item{mean}{a vector of means (if centering was requested)}
  \item{std}{a vector of standard deviations (if scaling or PCA was requested)}
  \item{rotation}{a matrix of eigenvectors if PCA was requested}
  \item{method}{the value of\code{method}}
  \item{thresh}{the value of\code{thresh}}
  \item{numComp}{the number of principal components required of capture the specified amount of variance}

}

\author{ Max Kuhn }

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{\code{\link[stats]{prcomp}}, \code{\link{spatialSign}}}

\examples{
data(BloodBrain)
# one variable has one unique value
\dontrun{preProc <- preProcess(bbbDescr[1:100,])}

preProc <- preProcess(bbbDescr[1:100,-3])
training <- predict(preProc, bbbDescr[1:100,-3])
test <- predict(preProc, bbbDescr[101:208,-3])
}

\keyword{ utilities }

