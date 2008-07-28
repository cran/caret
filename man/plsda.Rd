\name{plsda}
\alias{plsda.default}
\alias{predict.plsda}
\alias{plsda}
\title{Partial Least Squares Discriminant Analysis}
\description{
\code{plsda} is used to fit PLS models for classification.
}
\usage{
plsda(x, ...)

\method{plsda}{default}(x,  y, ncomp = 2, probMethod = "softmax", prior = NULL, ...)

\method{predict}{plsda}(object, newdata = NULL, ncomp = NULL, type = "class", ...)
}

\arguments{
  \item{x}{a matrix or data frame of predictors}
  \item{y}{a factor or indicator matrix for the discrete outcome. If a matrix, the entries must be either 0 or 1 and rows must sum to one}
  \item{ncomp}{the number of components to include in the model. Predictions can be made for models with values less than \code{ncomp}. }
  \item{probMethod}{either "softmax" or "Bayes" (see Details)}
  \item{prior}{a vector or prior probabilities for the classes (only used for \code{probeMethod = "Bayes"}}
  \item{\dots}{arguments to pass to \code{\link[pls]{plsr}} (code{plsda} only)}
  \item{object}{an object produced by \code{plsda}}
  \item{newdata}{a matrix or data frame of predictors}
  \item{type}{either \code{"class"}, \code{"prob"} or \code{"raw"} to produce the predicted class, class probabilities  or the raw model scores, respectively.}  
}
\details{
If a factor is supplied, the appropriate indicator matrix is created by \code{plsda}.

A multivariate PLS model is fit to the indicator matrix using the \code{\link[pls]{plsr}} function. 

Two prediciton methods can be used:
  \item{the softmax function}{The softmax functions transforms the model predictions to "probability-like" values (e.g. on  [0, 1] and sum to 1). The class with the largest class probability is the predicted class }  
  \item{Bayes rule }{Bayes rule can be applied to the model predictions to form posterior probabilities. Here, the model predictions for the training set are used along with the training set outcomes to create conditional distributions for each class. When new samples are predicted, the raw model predictions are run through these conditional distributions to produce a posterior probability for each class (along with the prior). This process is repeated \code{ncomp} times for every possible PLS model. The \code{\link[klaR]{NaiveBayes}} function is used with \code{usekernel = TRUE} for the posterior probability calculations.}

}
\value{
For \code{plsda}, an object of class "plsda" and "mvr". The predict
method produces either a vector, matrix or three-dimensional array,
depending on the values of \code{type} of \code{ncomp}. For example,
specifying more than one value of \code{ncomp} with \code{type =
  "class"} with produce a three dimensional array but the default
specification would produce a factor vector.
}

\seealso{\code{\link[pls]{plsr}}}
\examples{
data(mdrr)
set.seed(1)
inTrain <- sample(seq(along = mdrrClass), 450)
 
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]
 
preProcValues <- preProcess(training)

trainDescr <- predict(preProcValues, training)
testDescr <- predict(preProcValues, test)

useBayes   <- plsda(trainDescr, trainMDRR, ncomp = 5,
                    probMethod = "Bayes")
useSoftmax <- plsda(trainDescr, trainMDRR, ncomp = 5)

confusionMatrix(
                predict(useBayes, testDescr),
                testMDRR)

confusionMatrix(
                predict(useSoftmax, testDescr),
                testMDRR)

histogram(
          ~predict(useBayes, testDescr, type = "prob")[,"Active"]
          | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
histogram(
          ~predict(useSoftmax, testDescr, type = "prob")[,"Active",]
          | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))


# different sized objects are returned
length(predict(useBayes, testDescr))
dim(predict(useBayes, testDescr, ncomp = 1:3))
dim(predict(useBayes, testDescr, type = "prob"))
dim(predict(useBayes, testDescr, type = "prob", ncomp = 1:3))
}


\keyword{models}

