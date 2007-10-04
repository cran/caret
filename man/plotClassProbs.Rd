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

rpartFit <- train(trainData[, -5], trainData[, 5], "rpart", tuneLength = 15)
plsFit <- train(trainData[, -5], trainData[, 5], "pls", tuneLength = 9)
predProbs <- extractProb(list(plsFit, rpartFit),
   testX = testData[, -5], testY = testData[, 5])


plotClassProbs(predProbs)
plotClassProbs(predProbs[predProbs$model == "pls",])
plotClassProbs(predProbs[predProbs$model == "pls" & predProbs$dataType == "Test",])

# example plot as if this were a two class problem:
trainData <- iris[-(1:50),]
plsFit <- train(trainData[, -5], factor(trainData[, 5]), "pls", tuneLength = 9)
predProbs <- extractProb(list(plsFit))
plotClassProbs(predProbs)
}

\keyword{hplot}
