\name{trainControl}
\alias{trainControl}
\title{Control parameters for train}
\description{Control of printing and resampling for train}
\usage{
trainControl(
             method = "boot", 
             number = ifelse(method == "cv", 10, 25), 
             verboseIter = TRUE, 
             returnData = TRUE, 
             returnResamp = "final",
             p = 0.75, 
             summaryFunction = defaultSummary,
             selectionFunction = "best",
             index = NULL)
}
\arguments{
  \item{method}{The resampling method: \code{boot}, \code{cv},
    \code{LOOCV},  \code{LGOCV} (for repeated training/test splits), or
    \code{oob} (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis, or conditional tree forest models)}
  \item{number}{Either the number of folds or number of resampling iterations}
  \item{verboseIter}{A logical for printing a training log.}
  \item{returnData}{A logical for saving the data}
  \item{returnResamp}{A character string indicating how much of the resampled summary metrics should be saved. Values can be ``final'', ``all'' or ``none''}
  \item{p}{For leave-group out cross-validation: the training percentage}
  \item{summaryFunction}{a function to compute performance metrics across resamples. The arguments to the function should be the same as those in \code{\link{defaultSummary}}.}
  \item{selectionFunction}{the function used to select the optimal tuning parameter. This can be a name of the function or the funciton itself. See \code{\link{best}} for details and other options.}
  \item{index}{a list with elements for each resampling iteration. Each list element is the sample rows used for training at that iteration.}
}

\value{
 An echo of the parameters specified
}
\author{max Kuhn}

\keyword{utilities}
