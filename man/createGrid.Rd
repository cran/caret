\name{createGrid}
\alias{createGrid}
\title{Tuning Parameter Grid}
\description{
   This function creates a data frame that contains a grid of complexity
   parameters specific methods.
}
\usage{
  createGrid(method, len = 3, data = NULL, pp = NULL)
}
\arguments{
   \item{method}{a string specifying which classification model to use. See \code{\link{train}} for a full list.}
   \item{len}{an integer specifying the number of points on the grid for each tuning parameter. }
   \item{data}{the training data (only needed in the case where the \code{method} is \code{cforest}, \code{earth}, \code{bagEarth}, \code{fda}, \code{bagFDA}, \code{rpart}, \code{svmRadial}, \code{pam}, \code{lars2}, \code{rf} or  \code{pls}). The outcome should be in a column called \code{.outcome}.}
   \item{pp}{an optional vector of pre-processing options.}
}
\details{
A grid is created with rows corresponding to complexity parameter combinations. If the model does not use tuning parameters (like a linear model), values of \code{NA} are returned. Columns are named the same as the parameter name, but preceded by a period.

For some models (see list above), the data should be passed to the function via the \code{data} argument. In these cases, the outcome should be included in a column named \code{.outcome}.
}
\value{
 A data frame where the rows are combinations of tuning parameters and columns correspond to the parameters. The column names should be the parameter names preceded by a dot (e.g. \code{.mtry})
}
\author{Max Kuhn}
\seealso{\code{\link{train}}}
\examples{
createGrid("rda", 4)
createGrid("lm")
createGrid("nnet")

## data needed for SVM with RBF:
\dontrun{
tmp <- iris
names(tmp)[5] <- ".outcome"
head(tmp)
createGrid("svmRadial", data = tmp, len = 4)
}

}
\keyword{utilities}

