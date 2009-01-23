\name{plotClassProbs}
\alias{plotClassProbs}
\title{Plot Predicted Probabilities in Classification Models}
\description{
  This function takes an object (preferably from the function \code{\link{extractProb}})
  and creates a lattice plot.  
  
  If the call to \code{\link{extractProb}} included test data, these data are shown, but
  if unknowns were also included, these are not plotted
  
}
\usage{
plotClassProbs(object, ...)
}
\arguments{
  \item{object}{an object (preferably from the function \code{\link{extractProb}}. There 
  should be columns for each level of the class factor and columns named \code{obs}, \code{pred}, \code{model} (e.g. "rpart", "nnet" etc)
  and \code{dataType} (e.g. "Training", "Test" etc) }
  \item{\dots}{parameters to pass to   \code{\link[lattice]{histogram}}}
}

\value{
   A lattice object. Note that the plot has to be printed to be displayed (especially in a loop).
}
\author{Max Kuhn}

\examples{

data(iris)
set.seed(90)
inTrain <- sample(1:dim(iris)[1], 100)

trainData <- iris[inTrain,]
testData <- iris[-inTrain,]

rpartFit <- train(trainData[, -5], trainData[, 5], "rpart", 
                  tuneLength = 15)
ldaFit <- train(trainData[, -5], trainData[, 5], "lda")
predProbs <- extractProb(list(ldaFit, rpartFit),
                         testX = testData[, -5], 
                         testY = testData[, 5])

plotClassProbs(predProbs)
plotClassProbs(predProbs[predProbs$model == "lda",])
plotClassProbs(predProbs[predProbs$model == "lda" & 
                         predProbs$dataType == "Test",])

}

\keyword{hplot}
