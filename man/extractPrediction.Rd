\name{extractPrediction}
\alias{extractPrediction}
\alias{extractProb}
\title{Extract predictions and class probabilities from train objects}
\description{
This function loops through a number of train objects and calculates the
training and test data predictions and class probabilities
}
\usage{
extractPrediction(object, testX = NULL, testY = NULL, unkX = NULL, 
   unkOnly = !is.null(unkX) & is.null(testX), verbose = FALSE)

extractProb(object, testX = NULL,  testY = NULL,  unkX = NULL, 
   unkOnly = !is.null(unkX) & is.null(testX),  verbose = FALSE)
}
\arguments{
  \item{object}{a list of objects of the class \code{train}. The objects must have been generated with 
   \code{fitBest = FALSE} and \code{returnData = TRUE}.}
  \item{testX}{an optional set of data to predict}
  \item{testY}{an optional outcome corresponding to the data given in \code{testX}}
  \item{unkX}{another optional set of data to predict without known outcomes}  
  \item{unkOnly}{a logical to bypass training and test set predictions. This is useful if speed is needed
   for unknown samples.}
  \item{verbose}{a logical for printing messages}
}
\details{
The optimal tuning values given in the \code{tuneValue} slot of the \code{finalModel} object are used to predict.
}
\value{
  For \code{extractPrediction}, a data frame with columns:
  \item{obs }{the observed training and test data}
  \item{pred }{predicted values}
  \item{model}{the type of model used to predict}
  \item{dataType }{"Training", "Test" or "Unknown" depending on what was
    specified}
  For \code{extractProb}, a data frame. There is a column for each class
  containing the probabilities. The remaining columns are the same as
  above (although the \code{pred} column is the predicted class)  
}
\author{Max Kuhn}
\examples{
   \dontrun{
data(Satellite)
numSamples <- dim(Satellite)[1]
set.seed(716)

varIndex <- 1:numSamples

trainSamples <- sample(varIndex, 150)

varIndex <- (1:numSamples)[-trainSamples]
testSamples <- sample(varIndex, 100)

varIndex <- (1:numSamples)[-c(testSamples, trainSamples)]
unkSamples <- sample(varIndex, 50)

trainX <- Satellite[trainSamples, -37]
trainY <- Satellite[trainSamples, 37]

testX <- Satellite[testSamples, -37]
testY <- Satellite[testSamples, 37]

unkX <- Satellite[unkSamples, -37]

knnFit <- train(trainX, trainY, "knn")
rpartFit <- train(trainX, trainY, "rpart")

predTargets <- extractPrediction(list(knnFit, rpartFit), testX = testX, testY = testY, unkX = unkX)
   
  }
}
\keyword{manip}
