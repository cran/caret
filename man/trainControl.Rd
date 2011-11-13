\name{trainControl}
\alias{trainControl}
\title{Control parameters for train}
\description{Control the computational nuances of the \code{\link{train}} function}
\usage{
trainControl(method = "boot", 
             number = ifelse(method \%in\% c("cv", "repeatedcv"), 10, 25),
             repeats = ifelse(method \%in\% c("cv", "repeatedcv"), 1, number),
             verboseIter = FALSE, 
             returnData = TRUE, 
             returnResamp = "final",
             savePredictions = FALSE,
             p = 0.75, 
             classProbs = FALSE,
             summaryFunction = defaultSummary,
             selectionFunction = "best",
             custom = NULL,
             preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
             index = NULL,
             timingSamps = 0,
             predictionBounds = rep(FALSE, 2))
}
\arguments{
  \item{method}{The resampling method: \code{boot}, \code{boot632}, \code{cv}, \code{repeatedcv},
    \code{LOOCV},  \code{LGOCV} (for repeated training/test splits), or
    \code{oob} (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)}
  \item{number}{Either the number of folds or number of resampling iterations}
  \item{repeats}{For repeated k-fold cross-validation only: the number of complete sets of folds to compute}
  \item{verboseIter}{A logical for printing a training log.}
  \item{returnData}{A logical for saving the data}
  \item{returnResamp}{A character string indicating how much of the resampled summary metrics should be saved. Values can be ``final'', ``all'' or ``none''}
  \item{savePredictions}{a logical to save the hold-out predictions for each resample}
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{classProbs}{a logical; should class probabilities be computed for classification models (along with predicted values) in each resample?}
  \item{summaryFunction}{a function to compute performance metrics across resamples. The arguments to the function should be the same as those in \code{\link{defaultSummary}}.}
  \item{custom}{an optional list of functions that can be used to fit custom models. See the details below and worked examples in the "train" package vignette. This is an "experimental" version for testing. Please send emails to the maintainer for suggestions or problems.}
  \item{selectionFunction}{the function used to select the optimal tuning parameter. This can be a name of the function or the function itself. See \code{\link{best}} for details and other options.}
  \item{preProcOptions}{A list of options to pass to \code{\link{preProcess}}. The type of pre-processing (e.g. center, scaling etc) is passed in via the \code{preProc} option in \code{\link{train}}.}
  \item{index}{a list with elements for each resampling iteration. Each list element is the sample rows used for training at that iteration.}
  \item{timingSamps}{the number of training set samples that will be used to measure the time for predicting samples (zero indicates that the prediction time should not be estimated.}
  \item{predictionBounds}{a logical or numeric vector of length 2 (regression only). If logical, the predictions can be constrained to be within the limit of the training set outcomes. For example, a value of \code{c(TRUE, FALSE)} would only constrain the lower end of predictions. If numeric, specific bounds can be used. For example, if \code{c(10, NA)}, values below 10 would be predicted as 10 (with no constraint in the upper side).}
}

\value{
 An echo of the parameters specified
}

\details{
For custom modeling functions, several functions can be specified using the \code{custom} argument:
\itemize{
    \item{parameters}{a data frame or function of tuning parameters}
    \item{model}{a function that trains the model}
    \item{prediction}{a function that predicts new samples (either numbers or character/factor vectors)}
    \item{probability}{an optional function for classification models that returns a matrix or data frame of class probabilities (in columns)}
    \item{sort}{a function that sorts the tuning parameters by complexity}
    }

For more details and worked examples, see the ``caret Manual -- Model Building'' vignette.

}

\author{Max Kuhn}

\keyword{utilities}
