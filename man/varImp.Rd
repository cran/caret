\name{varImp}

\alias{varImp}
\alias{varImp.train}
\alias{varImp.earth}
\alias{varImp.rpart}
\alias{varImp.randomForest}
\alias{varImp.gbm}
\alias{varImp.regbagg}
\alias{varImp.classbagg}
\alias{varImp.pamrtrained}
\alias{varImp.lm}
\alias{varImp.mvr}
\alias{varImp.bagEarth}
\alias{varImp.RandomForest}

\title{Calculation of variable importance for regression and classification models}

\description{
A generic method for calculating variable importance for objects produced by
\code{train} and method specific methods
}

\usage{
\method{varImp}{train}(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...)
\method{varImp}{earth}(object, value = "gcv", ...)
\method{varImp}{rpart}(object, ...)
\method{varImp}{randomForest}(object, ...)
\method{varImp}{gbm}(object, numTrees, ...)
\method{varImp}{classbagg}(object, ...)
\method{varImp}{regbagg}(object, ...)
\method{varImp}{pamrtrained}(object, threshold, data, ...)
\method{varImp}{lm}(object, ...)
\method{varImp}{mvr}(object, ...)
\method{varImp}{bagEarth}(object, ...)
\method{varImp}{RandomForest}(object, normalize = TRUE, ...)
}

\arguments{
  \item{object}{an object corresponding to a fitted model}
  \item{useModel}{use a model based technique for measuring variable importance?
  This is only used for some models (lm, pls, rf, rpart, gbm, pam and mars)}  
  \item{nonpara}{should nonparametric methods be used to assess the relationship
  between the features and response (only used with \code{useModel = FALSE} and
  only passed to  \code{filterVarImp}).}  
  \item{scale}{should the importances be scaled to 0 and 100?} 
  \item{\dots}{parameters to pass to the specific \code{varImp} methods}
  \item{numTrees}{the number of iterations (trees) to use in a boosted tree model}        
  \item{threshold}{the shrinkage threshold (\code{pamr} models only)}        
   \item{data}{the training set predictors (\code{pamr} models only)}        
   \item{value}{the statistic that will be used to calculate importance:
     either \code{gcv}, \code{nsubsets}, or \code{rss}}
  \item{normalize}{a logical; should the OOB mean importance values be divided
    by their standard deviations?}
}

\value{
   A data frame with class \code{c("varImp.train", "data.frame")} for
   \code{varImp.train} or a matrix for other models.
 }

\details{
For models that do not have corresponding \code{varImp} methods, see
\code{filerVarImp}.

Otherwise:


   \item Linear Models: the absolute value of the t--statistic
   for each model parameter is used.
   
   \item Random Forest: \code{varImp.randomForest} and
   \code{varImp.RandomForest} are wrappers around the importance functions from the
   \pkg{randomForest} and \pkg{party} packages, respectively.
   
   \item Partial Least Squares: the variable importance measure here is based on 
   weighted sums of the absolute regression coefficients. The weights are a function of
   the reduction of the sums of squares across the number of PLS components and are 
   computed separately for each outcome. Therefore, the contribution of the coefficients
  are weighted proportionally to the reduction in the sums of squares.
  
   
  \item Recursive Partitioning: The reduction in the loss function
  (e.g. mean squared error) attributed to each variable at each split is 
  tabulated and the sum is returned. Also, since there may be candidate variables
  that are important but are not used in a split, the top competing variables are
  also tabulated at each split. This can be turned off using the \code{maxcompete}
  argument in \code{rpart.control}. This method does not currently provide
  class--specific measures of importance when the response is a factor.
  
  \item Bagged Trees: The same methodology as a single tree is applied to 
  all bootstrapped trees and the total importance is returned

  \item Boosted Trees: \code{varImp.gbm} is a wrapper around the function from that package (see the \pkg{gbm} package vignette)
  
  \item Multivariate Adaptive Regression Splines: MARS models 
        include a backwards elimination feature selection routine that
        looks at reductions in the generalized cross-validation (GCV)
        estimate of error. The \code{varImp} function tracks the changes in
        model statistics, such as the GCV, for each predictor and
        accumulates the reduction in the statistic when each
        predictor's feature is added to the model. This total reduction
        is used as the variable importance measure. If a predictor was
        never used in any of the MARS basis functions in the final model 
        (after pruning), it has an importance
        value of zero. Prior to June 2008, the package used an internal function 
        for these calculations. Currently, the \code{varImp}  is a wrapper to
        the \code{\link[earth]{evimp}}  function in the \code{earth} package. There are three statistics that can be used to
        estimate variable importance in MARS models. Using
        \code{varImp(object, value = "gcv")} tracks the reduction in the
        generalized cross-validation statistic as terms are added.
        However, there are some cases when terms are retained 
        in the model that result in an increase in GCV. Negative variable 
        importance values for MARS are set to zero. 
        Alternatively, using
        \code{varImp(object, value = "rss")} monitors the change in the
        residual sums of squares (RSS) as terms are added, which will
        never be negative. 
        Also, the option \code{varImp(object, value =" nsubsets")}, which 
        counts the number of subsets where the variable is used (in the final, 
        pruned model). 
   
  \item Nearest shrunken centroids: The difference between the class centroids and the overall centroid is used to measure the variable influence (see \code{pamr.predict}). The larger the difference between   the class centroid and the overall center of the data, the larger the separation between the classes. The training set predictions must be supplied when an object of class \code{pamrtrained} is given to \code{varImp}. 

  

}

\author{Max Kuhn}

\keyword{ models }

