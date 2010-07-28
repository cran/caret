\name{bag.default}
\alias{bag.default}
\alias{bag}
\alias{bagControl}
\alias{predict.bag}

\title{
A General Framework For Bagging
}
\description{
\code{bag} provides a framework for bagging classification or regression models. The user can provide their own functions for model building, prediction and aggregation of predictions (see Details below).
}
\usage{
bag(x, ...)

\method{bag}{default}(x, y, B = 10, vars = NULL, bagControl = bagControl(), ...)

bagControl(fit = NULL, 
           predict = NULL, 
           aggregate = NULL)


\method{predict}{bag}(object, newdata = NULL, ...)
}
\arguments{
  \item{x}{
a matrix or data frame of predictors
}
  \item{y}{
a vector of outcomes
}
  \item{B}{
the number of bootstrap samples to train over.
}
  \item{bagControl}{
a list of options.
}
  \item{\dots}{
arguments to pass to the model function
}
  \item{fit}{
a function that has arguments \code{x}, \code{y} and \code{...} and produces a model object that can later be used for prediction
}
  \item{predict}{
a function that generates predictions for each sub-model. The function should have arguments \code{object} and \code{x}. The output of the function can be any type of object (see the example below where posterior probabilities are generated)
}
  \item{aggregate}{
a function with arguments \code{x} and \code{type}. The function that takes the output of the \code{predict} function and reduces the bagged predictions to a single prediction per sample. the \code{type} argument can be used to switch between predicting classes or class probabilities for classification models.
}
  \item{vars}{
an integer. If this argument is not \code{NULL}, a random sample of size \code{vars} is taken of the predictors in each bagging iteration. If \code{NULL}, all predictors are used. 
}
  \item{object}{
an object of class \code{bag}.
}
  \item{newdata}{
a matrix or data frame of samples for prediction. Note that this argument must have a non-null value
}
}

\details{
The function is basically a framework where users can plug in any model in to assess the effect of bagging.

One note: when \code{vars} is not \code{NULL}, the sub-setting occurs prior to the \code{fit} and \code{predict} functions are called. In this way, the user probably does not need to account for the change in predictors in their functions.

When using \code{bag} with \code{\link{train}}, classification models should use \code{type = "prob"} inside of the \code{predict} function so that \code{predict.train(object, newdata, type = "prob")} will work.

}


\value{
\code{bag} produces an object of class \code{bag} with elements
  \item{fits }{a list with two sub-objects: the \code{fit} object has the actual model fit for that bagged samples and the \code{vars} object is either \code{NULL} or a vector of integers corresponding to which predictors were sampled for that model}
  \item{control }{a mirror of the arguments passed into \code{bagControl}}
  \item{call }{the call}
  \item{B }{the number of bagging iterations}
  \item{dims }{the dimensions of the training set}

}

\author{
Max Kuhn
}


\examples{
## A simple example of bagging conditional inference regression trees:
data(BloodBrain)

## Fit a model with the default values
ctreeFit <- function(x, y, ...)
  {
    library(party)
    data <- as.data.frame(x)
    data$y <- y
    ctree(y~., data = data)
  }

## Generate simple predictions of the outcome
ctreePred <- function(object, x)
  {
    predict(object, x)[,1]
  }

## Take the median of the bagged predictions
ctreeAg <- function(x, type = NULL)
  {
    ## x is a list of vectors, so we convert them to a matrix
    preds <- do.call("cbind", x)
    apply(preds, 1, median)
  }

treebag <- bag(bbbDescr, logBBB, B = 10,
               bagControl = bagControl(fit = ctreeFit,
                                       predict = ctreePred,
                                       aggregate = ctreeAg))




## An example of pooling posterior probabilities to generate class predictions
data(mdrr)

## remove some zero variance predictors and linear dependencies
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .95)]


## The fit and predict functions are stright-forward:
ldaFit <- function(x, y, ...)
  {
    library(MASS)
    lda(x, y, ...)
  }

ldaPred <- function(object, x)
  {
    predict(object, x)$posterior
  }


## For the aggregation function, we take the median of the bagged
## posterior probabilities and pick the largest as the class
ldaAg <- function(x, type = "class")
  {
    ## The class probabilities come in as a list of matrices
    ## For each class, we can pool them then average over them

    pooled <- x[[1]] & NA
    classes <- colnames(pooled)
    for(i in 1:ncol(pooled))
      {
        tmp <- lapply(x, function(y, col) y[,col], col = i)
        tmp <- do.call("rbind", tmp)
        pooled[,i] <- apply(tmp, 2, median)
      }
    if(type == "class")
      {
        out <- factor(classes[apply(pooled, 1, which.max)],
                      levels = classes)
      } else out <- pooled
    out
  }


bagLDA <- bag(mdrrDescr, mdrrClass, 
              B = 10,
              vars = 10,
              bagControl = bagControl(fit = ldaFit,
                                      predict = ldaPred,
                                      aggregate = ldaAg))

\dontrun{
basicLDA <- train(mdrrDescr, mdrrClass, "lda")

bagLDA2 <- train(mdrrDescr, mdrrClass, 
                 "bag", 
                 B = 10, 
                 bagControl(fit = ldaFit,
                            predict = ldaPred,
                            aggregate = ldaAg),
                 tuneGrid = data.frame(.vars = c((1:10)*10 , ncol(mdrrDescr))))
}
}
\keyword{models }
