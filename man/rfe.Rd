\name{rfe}
\alias{rfe}
\alias{rfe.default}
\alias{rfeIter}
\title{Backwards Feature Selection}
\description{
A simple backwards selection, a.k.a. recursive feature selection (RFE), algorithm
}
\usage{
rfe(x, ...)

\method{rfe}{default}(x, y, 
    sizes = 2^(2:4), 
    rfeControl = rfeControl(), 
    ...)

rfeIter(x, y, 
        testX, testY, 
        sizes, 
        rfeControl = rfeControl(), 
        ...)
}

\arguments{
  \item{x}{a matrix or data frame of predictors for model training. This object must have unique column names.}
  \item{y}{a vector of training set outcomes (either numeric or factor) }
  \item{testX}{a matrix or data frame of test set predictors. This must have the same column names as \code{x}}
  \item{testY}{a vector of test set outcomes }
  \item{sizes}{a numeric vector of integers corresponding to the number of features that should be retained}
  \item{rfeControl}{a list of options, including functions for fitting and prediction. See the package vignette or \code{\link{rfeControl}} for examples}
  \item{\dots}{options to pass to the model fitting function}
}
\details{
This function implements backwards selection of predictors based on predictor importance ranking. The predictors are ranked and the less important ones are sequentially eliminated. The package vignette for feature selection has detailed descriptions of the algorithms.

\code{rfeIter} is the basic algorithm while \code{rfe} wraps these operations inside of resampling. To avoid selection bias, it is better to use the function \code{rfe} than \code{rfeIter}.
}
\value{
A list with elements
  \item{finalVariables}{a list of size \code{length(sizes) + 1} containing the column names of the ``surviving'' predictors
           at each stage of selection. The first element corresponds to all the predictors (i.e. \code{size = ncol(x)})}
  \item{pred }{a data frame with columns for the test set outcome, the predicted outcome and the subset size.}

}

\author{Max Kuhn}

\seealso{\code{\link{rfeControl}}}
\examples{
\dontrun{
data(BloodBrain)

x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
x <- x[, -findCorrelation(cor(x), .8)]
x <- as.data.frame(x)

set.seed(1)
lmProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         number = 200))
set.seed(1)
lmProfile2 <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         rerank = TRUE, 
                                         number = 200))

xyplot(lmProfile$results$RMSE + lmProfile2$results$RMSE +
       rfProfile$results$RMSE + rfProfile2$results$RMSE ~ 
       lmProfile$results$Variables, 
       type = c("g", "p", "l"), 
       auto.key = TRUE)

rfProfile <- rfe(x, logBBB,
                 sizes = c(2, 5, 10, 20),
                 rfeControl = rfeControl(functions = rfFuncs))

bagProfile <- rfe(x, logBBB,
                  sizes = c(2, 5, 10, 20),
                  rfeControl = rfeControl(functions = treebagFuncs))

set.seed(1)
svmProfile <- rfe(x, logBBB,
                  sizes = c(5, 20, 65),
                  rfeControl = rfeControl(functions = caretFuncs, 
                                          number = 200),
                  ## pass options to train()
                  method = "svmRadial",
                  fit = FALSE)

## classification with no resampling

data(mdrr)
mdrrDescr <- scale(mdrrDescr[,-nearZeroVar(mdrrDescr)])
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]

set.seed(1)
inTrain <- createDataPartition(mdrrClass, p = .75, list = FALSE)[,1]

train <- mdrrDescr[ inTrain, ]
test  <- mdrrDescr[-inTrain, ]
trainClass <- mdrrClass[ inTrain]
testClass  <- mdrrClass[-inTrain]

preProc <- preProcess(train)
train <- predict(preProc, train)
test  <- predict(preProc, test)

nbProfile <- rfeIter(train, trainClass,
                     test,  testClass, 
                     sizes = c(1:10, 15, 30),
                     rfeControl = rfeControl(functions = nbFuncs))

splitUp <- split(nbProfile$pred, 
                 factor(nbProfile$pred$subset))
testResults <- lapply(splitUp, 
                      function(u) postResample(u$pred, u$obs))
Variables <- as.numeric(names(testResults))

testResults <- do.call("rbind", testResults)
testResults <- cbind(testResults, Variables)
plot(testResults[,3], testResults[,1])
}
}

\keyword{models}
