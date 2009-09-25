\name{predictors}
\alias{predictors}
\alias{predictors.BinaryTree}
\alias{predictors.LogitBoost}
\alias{predictors.NaiveBayes}
\alias{predictors.RandomForest}
\alias{predictors.Weka_classifier}
\alias{predictors.bagEarth}
\alias{predictors.bagFDA}
\alias{predictors.blackboost}
\alias{predictors.classbagg}
\alias{predictors.earth}
\alias{predictors.fda}
\alias{predictors.formula}
\alias{predictors.gamboost}
\alias{predictors.gausspr}
\alias{predictors.gbm}
\alias{predictors.glmboost}
\alias{predictors.gpls}
\alias{predictors.knn3}
\alias{predictors.ksvm}
\alias{predictors.lda}
\alias{predictors.list}
\alias{predictors.lm}
\alias{predictors.lssvm}
\alias{predictors.multinom}
\alias{predictors.mvr}
\alias{predictors.nnet}
\alias{predictors.pamrtrained}
\alias{predictors.pcaNNet}
\alias{predictors.ppr}
\alias{predictors.randomForest}
\alias{predictors.rda}
\alias{predictors.regbagg}
\alias{predictors.rpart}
\alias{predictors.rvm}
\alias{predictors.slda}
\alias{predictors.superpc}
\alias{predictors.terms}
\alias{predictors.train}
\alias{predictors.rfe}
\title{ List predictors used in the model}
\description{
This class uses a model fit to determine which predictors were used in the final model.
}
\usage{
predictors(x, ...)

\method{predictors}{train}(x,  ...)

\method{predictors}{terms}(x,  ...)

\method{predictors}{formula}(x,  ...)

\method{predictors}{list}(x,  ...)

\method{predictors}{mvr}(x,  ...)

\method{predictors}{gbm}(x,  ...)

\method{predictors}{Weka_classifier}(x,  ...)

\method{predictors}{fda}(x,  ...)

\method{predictors}{earth}(x,  ...)

\method{predictors}{gausspr}(x,  ...)

\method{predictors}{ksvm}(x,  ...)

\method{predictors}{lssvm}(x,  ...)

\method{predictors}{rvm}(x,  ...)

\method{predictors}{train}(x,  ...)

\method{predictors}{gpls}(x,  ...)

\method{predictors}{knn3}(x,  ...)

\method{predictors}{LogitBoost}(x,  ...)

\method{predictors}{lda}(x,  ...)

\method{predictors}{rda}(x,  ...)

\method{predictors}{multinom}(x,  ...)

\method{predictors}{nnet}(x,  ...)

\method{predictors}{pcaNNet}(x,  ...)

\method{predictors}{NaiveBayes}(x,  ...)

\method{predictors}{randomForest}(x,  ...)

\method{predictors}{pamrtrained}(x, newdata = NULL, threshold = NULL, ...)

\method{predictors}{superpc}(x, newdata = NULL, threshold = NULL, n.components = NULL, ...)

\method{predictors}{slda}(x,  ...)

\method{predictors}{rpart}(x, surrogate = TRUE,  ...)

\method{predictors}{regbagg}(x, surrogate = TRUE,  ...)

\method{predictors}{classbagg}(x, surrogate = TRUE,  ...)

\method{predictors}{glmboost}(x,  ...)

\method{predictors}{gamboost}(x,  ...)

\method{predictors}{blackboost}(x,  ...)

\method{predictors}{BinaryTree}(x, surrogate = TRUE,  ...)

\method{predictors}{RandomForest}(x, surrogate = TRUE,  ...)

\method{predictors}{bagEarth}(x,  ...)

\method{predictors}{bagFDA}(x,  ...)

\method{predictors}{ppr}(x,  ...)

\method{predictors}{rfe}(x,  ...)
}

\arguments{
  \item{x}{ a model object, list or terms}
  \item{newdata}{ for \code{\link[pamr]{pamr.train}} and \code{\link[superpc]{superpc.train}}: the training data }
  \item{threshold}{for \code{\link[pamr]{pamr.train}} and \code{\link[superpc]{superpc.train}}: the feature selection threshold}
  \item{n.components}{for \code{\link[superpc]{superpc.train}}: the number of PCA components used}
  \item{surrogate}{a logical for \code{\link[rpart]{rpart}}, \code{\link[ipred:bagging]{ipredbagg}},  \code{\link[party]{ctree}} and \code{\link[party]{cforest}}: should variables used as surrogate splits also be returned?}
  \item{\dots}{ not currently used }
}
\details{
For \code{\link[randomForest]{randomForest}}, \code{\link[party]{cforest}}, \code{\link[party]{ctree}}, \code{\link[rpart]{rpart}}, \code{\link[ipred:bagging]{ipredbagg}},  \code{\link[ipred]{bagging}},  \code{\link[earth]{earth}},  \code{\link[mda]{fda}},  \code{\link[pamr]{pamr.train}},   \code{\link[superpc]{superpc.train}},  \code{\link{bagEarth}} and  \code{\link{bagFDA}}, an attempt was made to report the predictors that were actually used in the final model. 

In cases where the predictors cannot be determined, \code{NA} is returned. For example, \code{\link[nnet]{nnet}} may retrun missing values form \code{predictors}.
}
\value{
a character string of predictors or \code{NA}.
}


\keyword{models }

