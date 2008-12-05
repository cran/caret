\name{createDataPartition}
\alias{createDataPartition}
\alias{createResample}
\alias{createFolds}
\title{Data Splitting functions}
\description{
  A series of test/training partitions are created using
  \code{createDataPartition} while \code{createResample} creates one or
  more bootstrap samples. \code{createFolds} splits the data into
  \code{k} groups.
}
\usage{
createDataPartition(y, 
                    times = 1,
                    p = 0.5,
                    list = TRUE,
                    groups = min(5, length(y)))
createResample(y, times = 10, list = TRUE)
createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
}
\arguments{
  \item{y}{a vector of outcomes}
  \item{times}{the number of partitions to create}
  \item{p}{the percentage of data that goes to training}
  \item{list}{logical - should the results be in a list (\code{TRUE}) or a matrix 
  with the number of rows equal to \code{floor(p * length(y))} and \code{times}
  columns.}
  \item{groups}{for numeric \code{y}, the number of breaks in the quantiles
    (see below)}
  \item{k}{an integer for the number of folds.}
  \item{returnTrain}{a logical. When true, the values returned are the
    sample positions corresponding to the data used during
    training. This argument only works in conjunction with \code{list = TRUE}}  
}
\details{

  For bootstrap samples, simple random sampling is used.

  For other data splitting,  the random sampling is done within the
  levels of \code{y} when \code{y} is a factor in an attempt to balance
  the class distributions within the splits.   For numeric \code{y}, the
  sample  is split into \code{groups} sections based
  on quantiles and sampling is done within these subgroups. Also, for
  very  small class sizes (<= 3) the classes may not show up in both the
  training and test data
}
\value{
   A list or matrix of row position integers corresponding to the training
   data
}
\author{Max Kuhn}

\examples{
data(oil)
createDataPartition(oilType, 2)

x <- rgamma(50, 3, .5)
inA <- createDataPartition(x, list = FALSE)

plot(density(x[inA]))
rug(x[inA])

points(density(x[-inA]), type = "l", col = 4)
rug(x[-inA], col = 4)

createResample(oilType, 2)

createFolds(oilType, 10)
createFolds(oilType, 5, FALSE)

createFolds(rnorm(21))
}
\keyword{utilities}


