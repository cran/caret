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

\method{plsda}{default}(x,  y, ncomp = 2, ...)

\method{predict}{plsda}(object, newdata = NULL, ncomp = NULL, type = "class", ...)

}

\arguments{
  \item{x}{a matrix or data frame of predictors}
  \item{y}{a factor or indicator matrix for the discrete outcome. If a matrix, the entries must be either 0 or 1 and rows must add to one}
  \item{ncomp}{the number of components to include in the model }
  \item{\dots}{arguments to pass to \code{\link[pls]{plsr}} (code{plsda} only)}
  \item{object}{an object produced by \code{plsda}}
  \item{newdata}{a matrix or data frame of predictors}
  \item{type}{either \code{"class"}, \code{"prob"} or \code{"raw"} to produce the predicted class, class probabilities  or the raw model scores, respectively.}  
}
\details{
If a factor is supplied, the appropriate indicator matrix is created by \code{plsda}.

A multivariate PLS model is fit to the indicator matrix using the \code{\link[pls]{plsr}} function. 

To predict, the softmax function is used to normalize the model output into probability-like scores. The class with the largest score is the assigned output class.
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

tmpX <- scale(mdrrDescr)

plsFit <- plsda(tmpX, mdrrClass, ncomp = 3)

table(predict(plsFit), mdrrClass)
}


\keyword{models}

