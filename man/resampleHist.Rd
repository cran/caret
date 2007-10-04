\name{resampleHist}
\alias{resampleHist}
\title{Plot the resampling distribution of the model statistics}
\description{
Create a lattice histogram or densityplot from the resampled outcomes from a \code{train} object.
}
\usage{
resampleHist(object, type = "density", ...)
}
\arguments{
  \item{object}{an object resulting form a call to \code{\link{train}}}
  \item{type}{a character string. Either "hist" or "density"}
  \item{\dots}{options to pass to histogram or densityplot}
}
\details{
All the metrics from the object are plotted.
}
\value{
a object of class \code{trellis}
}
\author{Max Kuhn}

\seealso{\code{\link{train}}, \code{\link[lattice]{histogram}}, \code{\link[lattice]{densityplot}}}
\examples{
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses, "knn")

resampleHist(knnFit)
}
\keyword{hplot}
