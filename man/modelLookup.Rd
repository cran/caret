\name{modelLookup}
\alias{modelLookup}
\title{
Descriptions Of Models Available in train()
}
\description{
This function enumerates the parameters and characteristics for models used
by \code{\link{train}}
}
\usage{
modelLookup(model = NULL)
}
\arguments{
  \item{model}{
a character string associated with the \code{method} argument of \code{\link{train}}. If no value is 
passed, all models are returned
}
}
\details{
One characteristic listed in the output is whether or not sub-models can be used for prediction. For example,
if a PLS model is fit with X components, PLS models with <X components do not need to be fit to obtain
predictions. \code{\link{train}} exploits this characteristic whenever possible.

These types of tuning parameters are defined as "sequential" parameters since one value can be used to derive a 
sequences of predictions. Examples of model codes that include sequential tuning parameters are; \code{blackboost}, 
\code{ctree}, \code{earth}, \code{enet}, \code{foba}, \code{gamboost}, \code{gbm}, \code{glmboost}, 
\code{glmnet}, \code{lars}, \code{lars2}, \code{lasso}, \code{logitBoost}, \code{pam}, \code{partDSA}, 
\code{pcr}, \code{pls}, \code{relaxo}, \code{rpart}, \code{scrda} and \code{superpc}.

}
\value{
a data frame with columns
  \item{model }{a character string for the model code}
  \item{parameter }{the tuning parameter name}
  \item{label }{a tuning parameter label (used in plots)}
  \item{seq }{a logical; can sub-models be used to decrease training time (see the Details section)}
  \item{forReg }{a logical; can the model be used for regression?}
  \item{forClass }{a logical; can the model be used for classification?}
  \item{probModel }{a logical; does the model produce class probabilities?}

}

\author{
Max Kuhn
}


\seealso{
\code{\link{train}}
}
\examples{
modelLookup()

modelLookup("gbm")

}

\keyword{ utilities }

