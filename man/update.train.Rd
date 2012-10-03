\name{update.train}
\alias{update.train}
\title{
Update and Re-fit a Model
}
\description{
\code{update} allows a user to over-ride the tuning parameter selection process by specifying a set of tuning parameters.
}
\usage{
\method{update}{train}(object, param = NULL, ...)
}
\arguments{
  \item{object}{
an object of class \code{\link{train}}
}
  \item{param}{
a data frame or named list of all tuning parameters
}
  \item{\dots}{
not currently used
}
}
\details{
To update the model, the training data must be stored in the model object (see the option \code{returnData} in \code{\link{trainControl}}. Also, all tuning parameters must be specified (with the preceding dot in the name).

All other options are held constant, including the original pre-processing (if any), options passed in using code{...} and so on.

When printing, the verbiage "The tuning parameter was set manually." is used to describe how the tuning parameters were created.
}
\value{
a new \code{\link{train}} object
}

\author{
Max Kuhn
}

\seealso{
 \code{\link{train}},  \code{\link{trainControl}}
}
\examples{
\dontrun{
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

update(knnFit1, list(.k = 3))
}
}

\keyword{ models }


