\name{predict.train}
\alias{predict.list}
\alias{predict.train}
\alias{extractPrediction}
\alias{extractProb}
\title{Extract predictions and class probabilities from train objects}
\description{
These functions can be used for a single \code{train} object or to loop through a number of \code{train} objects to calculate the
training and test data predictions and class probabilities.
}
\usage{

\method{predict}{list}(object, ...)

\method{predict}{train}(object, newdata = NULL, type = "raw", ...)

extractPrediction(models,
                  testX = NULL, testY = NULL,
                  unkX = NULL, 
                  unkOnly = !is.null(unkX) & is.null(testX),
                  verbose = FALSE)

extractProb(models,
            testX = NULL,  testY = NULL,
            unkX = NULL, 
            unkOnly = !is.null(unkX) & is.null(testX),
            verbose = FALSE)
}
\arguments{
  \item{object}{For \code{predict.train}, an object of class \code{\link{train}}. For \code{predict.list}, a list of objects of class \code{\link{train}}. }
  \item{newdata}{an optional set of data to predict on. If \code{NULL}, then the original training data are used}
  \item{type}{either "raw" or "prob", for the number/class predictions or class probabilities, respectively. Class probabilities are not available  for all classification models}
  \item{models}{a list of objects of the class \code{train}. The objects must have been generated with 
   \code{fitBest = FALSE} and \code{returnData = TRUE}.}
  \item{testX}{an optional set of data to predict}
  \item{testY}{an optional outcome corresponding to the data given in \code{testX}}
  \item{unkX}{another optional set of data to predict without known outcomes}  
  \item{unkOnly}{a logical to bypass training and test set predictions. This is useful if speed is needed
   for unknown samples.}
  \item{verbose}{a logical for printing messages}
  \item{\ldots}{additional arguments to be passed to other methods}
}
\details{
These functions are wrappers for the specific prediction functions in each modeling package. In each case, the optimal tuning values given in the \code{tuneValue} slot of the \code{finalModel} object are used to predict.

To get simple predictions for a new data set, the \code{predict} function can be used. 

To get predictions for a series of models at once, a list of \code{\link{train}} objects can be passes to the \code{predict} function and a list of model predictions will be returned.

The two extraction functions can be used to get the predictions and observed outcomes at once for the training, test and/or unknown samples at once in a single data frame (instead of a list of just the predictions). These objects can then be passes to \code{\link{plotObsVsPred}} or \code{\link{plotClassProbs}}.

}
\value{

  For \code{predict.train}, a vector of predictions if \code{type =  "raw"} or a data frame of class probabilites for \code{type =  "probs"}. In the latter case, there are columns for each class.

  For \code{predict.list}, a list results. Each element is produced by \code{predict.train}.

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

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{\code{\link{plotObsVsPred}}, \code{\link{plotClassProbs}}}

\examples{
   \dontrun{
library(mlbench)
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

predict(knnFit)
predict(knnFit, newdata = testX)
predict(knnFit, type = "prob")

bothModels <- list(
                   knn = knnFit,
                   tree = rpartFit)

predict(bothModels)

predTargets <- extractPrediction(
                                 bothModels,
                                 testX = testX,
                                 testY = testY,
                                 unkX = unkX)
   
  }
}
\keyword{manip}
