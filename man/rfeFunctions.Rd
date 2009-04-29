\name{rfe}
\alias{caretFuncs}
\alias{lmFuncs}
\alias{rfFuncs}
\alias{treebagFuncs}
\alias{ldaFuncs}
\alias{nbFuncs}
\alias{pickSizeBest}
\alias{pickSizeTolerance}
\alias{pickVars}

\title{Backwards Feature Selection Helper Functions}
\description{
Ancillary fuctions for backwards selection
}
\usage{
pickSizeTolerance(x, metric, tol = 1.5, maximize) 
pickSizeBest(x, metric, maximize) 

pickVars(y, size) 

caretFuncs
lmFuncs
rfFuncs
treebagFuncs
ldaFuncs
nbFuncs
}

\arguments{
  \item{x}{a matrix or data frame with the performance metric of interest}
  \item{metric}{a character string with the name of the performance metric that
                should be used to choose the appropriate number of variables }
  \item{maximize}{a logical; should the metric be maximized? }
  \item{tol}{a scalar to denote the acceptable difference in optimal performance 
             (see Details below) }
  \item{size}{an integer for the number of variables to retain}
}
\details{
This page describes the functions that are used in backwards selection (aka recursive 
feature elimination). The funcitons described here are passed to the algorithm via the 
\code{functions} argument of \code{\link{rfeControl}}. 

See \code{\link{rfeControl}} for details on how these functions should be defined.
}

\author{Max Kuhn}

\seealso{\code{\link{rfeControl}}, \code{\link{rfe}}}

\keyword{models}
