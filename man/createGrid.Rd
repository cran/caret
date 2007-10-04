\name{createGrid}
\alias{createGrid}
\title{Tuning Parameter Grid}
\description{
   This function creates a data frame that contains a grid of complexity
      parameters specific methods.
}
\usage{
createGrid(method, len = 3, data = NULL)
}
\arguments{
  \item{method}{a string specifying which classification model to use. Possible values are: \code{lm}, \code{rda}, \code{lda}, \code{gbm}, \code{rf},  \code{nnet}, \code{multinom}, \code{gpls}, \code{lvq}, \code{rpart}, \code{knn}, \code{pls}, \code{pam}, \code{nb},  \code{earth}, \code{treebag},
\code{svmpoly}, \code{svmradial}, \code{fda}, \code{bagEarth}, \code{bagFDA}, \code{glmboost}, \code{gamboost}, \code{blackboost},  \code{ada}, \code{ctree} and  \code{cforest}}
  \item{len}{an integer specifying the number of points on the grid for each tuning parameter. }
  \item{data}{the training data (only needed in the case where the \code{method} is \code{rpart},  \code{rf} or  \code{pls}.}
}
\details{
   A grid is created with rows corresponding to complexity parameter combinations. If the model does not use tuning parameters (like a linear model), values of \code{NA} are returned. Columns are named the same as the parameter name, but preceded by a period.
}
\value{
A data frame
}
\author{Max Kuhn}
\seealso{\code{\link{train}}}
\examples{
createGrid("rda", 4)
createGrid("lm")
createGrid("nnet")
}
\keyword{utilities}

