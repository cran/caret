\name{confusionMatrix}
\alias{confusionMatrix.default}
\alias{confusionMatrix}
\title{Create a confusion matrix}
\description{
Calculates a cross-tabulation of observed and predicted classes with associated statistics.
}
\usage{
\method{confusionMatrix}{default}(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), ...)
}
\arguments{
  \item{data}{a factor of predicted classes}
  \item{reference}{a factor of classes to be used as the true results}
  \item{positive}{an optional character string for the factor level that corresponds to a "positive" result (if that makes sense for your data). If there are only two factor levels, the first level will be used as the "positive" result.}
  \item{dnn}{a character vector of dimnames for the table}
  \item{\dots}{options to be passed to \code{table}. NOTE: do not include \code{dnn} here}
}
\details{
The functions requires that the factors have exactly the same levels.

For two class problems, the sensitivity, specificity, positive
predictive value and negative predictive value is calculated using the
\code{positive} argument. For more than two classes, these results are
calculated comparing each factor level to the remaining levels
(i.e. a "one versus all" approach). In each case, the overall accuracy and Kappa statistic are calculated.

The overall accuracy rate is computed along with a 95 percent confidence interval for this rate (using \code{\link[stats]{binom.test}}) and a one-sided test to see if the accuracy is better than the "no information rate," which is taken to be the largest class percentage in the data.
 
}
\value{
a list with elements
  \item{table}{the results of \code{table} on \code{data} and  \code{reference}}
  \item{positive}{the positive result level}
  \item{overall}{a numeric vector with overall accuracy and Kappa statistic values}
  \item{byClass}{the sensitivity, specificity, positive predictive value and negative predictive value for each class. For two class systems, this is calculated once using the \code{positive} argument}

}
\author{Max Kuhn}

\seealso{\code{\link{sensitivity}},   \code{\link{specificity}}, \code{\link{posPredValue}}, \code{\link{negPredValue}}, \code{\link{print.confusionMatrix}}, \code{\link[stats]{binom.test}}}
\examples{
numLlvs <- 4
confusionMatrix(
   factor(sample(rep(letters[1:numLlvs], 200), 50)),
   factor(sample(rep(letters[1:numLlvs], 200), 50)))  
   
numLlvs <- 2
confusionMatrix(
   factor(sample(rep(letters[1:numLlvs], 200), 50)),
   factor(sample(rep(letters[1:numLlvs], 200), 50)))     
}
\keyword{utilities}

