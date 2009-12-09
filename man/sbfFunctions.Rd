\name{caretSBF}
\alias{caretSBF}
\alias{lmSBF}
\alias{rfSBF}
\alias{treebagSBF}
\alias{ldaSBF}
\alias{nbSBF}
\alias{gamFilter}
\alias{anovaFilter}


\title{Selection By Filtering (SBF) Helper Functions}
\description{
Ancillary functions for univariate feature selection
}
\usage{
anovaFilter(x, y, cut = 0.05)
gamFilter(x, y, cut = 0.05) 

caretSBF
lmSBF
rfSBF
treebagSBF
ldaSBF
nbSBF
}

\arguments{
  \item{x}{a matrix or data frame of numeric predictors}
  \item{y}{a numeric or factor vector of outcomes}
  \item{cut}{a p-value cut-off }
}
\details{
This page documents the functions that are used in selection by filtering (SBF). The functions described here are passed to the algorithm via the 
\code{functions} argument of \code{\link{sbfControl}}. 

See \code{\link{sbfControl}} for details on how these functions should be defined.

\code{anovaFilter} and \code{gamFilter} are two examples of univariate filtering functions. \code{anovaFilter} fits a simple linear model between a single feature and the outcome, then the p-value for the whole model F-test is generated. If the p-values is greater than 0.05, the feature is retained for the model. \code{gamFilter} fits a generalized additive model between a single predictor and the outcome using a smoothing spline basis function. A p-value is generated using the whole model test from \code{\link[gam]{summary.gam}} and p-values greater than 0.05 indicate that a predictor will be excluded.

If a particular model fails for \code{lm} or \code{gam}, the predictor is not used in the model.

}

\author{Max Kuhn}

\seealso{\code{\link{sbfControl}}, \code{\link{sbf}}, \code{\link[gam]{summary.gam}}}

\keyword{models}
