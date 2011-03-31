\name{trainControl}
\alias{trainControl}
\title{Control parameters for train}
\description{Control the computational nuances of the \code{\link{train}} function}
\usage{
trainControl(method = "boot", 
             number = ifelse(method \%in\% c("cv", "repeatedcv"), 10, 25),
             repeats = ifelse(method \%in\% c("cv", "repeatedcv"), 1, number),
             verboseIter = TRUE, 
             returnData = TRUE, 
             returnResamp = "final",
             p = 0.75, 
             classProbs = FALSE,
             summaryFunction = defaultSummary,
             selectionFunction = "best",
             PCAthresh = 0.95,
             ICAcomp = 3,
             k = 5,
             index = NULL,
             workers = 1,
             computeFunction = lapply,
             computeArgs = NULL)
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
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{classProbs}{a logical; should class probabilities be computed for classification models (along with predicted values) in each resample?}
  \item{summaryFunction}{a function to compute performance metrics across resamples. The arguments to the function should be the same as those in \code{\link{defaultSummary}}.}
  \item{selectionFunction}{the function used to select the optimal tuning parameter. This can be a name of the function or the function itself. See \code{\link{best}} for details and other options.}
  \item{PCAthresh}{When invoking \code{\link{train}} with the \code{preProcess = "pca"} option, this parameter allows the user to determine how many PCA components should be kept on the basis of the cumulative amount of variance explained. See \code{\link{preProcess}} for more details.}
  \item{ICAcomp}{When invoking \code{\link{train}} with the \code{preProcess = "ica"} option, this parameter allows the user to determine how many ICA components should be kept. See \code{\link{preProcess}} for more details.}
  \item{k}{When invoking \code{\link{train}} with the \code{preProcess = "knnImpute"} option, this parameter allows the user to determine how many neighbors should be used for imputation. See \code{\link{preProcess}} for more details.}
  \item{index}{a list with elements for each resampling iteration. Each list element is the sample rows used for training at that iteration.}
  \item{workers}{an integer that specifies how many machines/processors will be used}
  \item{computeFunction}{a function that is \code{\link[base]{lapply}} or emulates \code{lapply}. It must have arguments \code{X}, \code{FUN} and \code{...}. \code{computeFunction} can be used to build models in parallel. See the examples in \code{link{train}}.}
  \item{computeArgs}{Extra arguments to pass into the \code{...} slot in \code{computeFunction}. See the examples in \code{\link{train}}.}
}

\value{
 An echo of the parameters specified
}
\author{Max Kuhn}

\keyword{utilities}
