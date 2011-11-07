\name{train}
\alias{train}
\alias{train.default}
\alias{train.formula}
\title{Fit Predictive Models over Different Tuning Parameters}
\description{
  This function sets up a grid of tuning parameters for a number of classification and regression routines,
  fits each model and calculates a resampling based performance measure. 
}
\usage{
train(x, ...)

\method{train}{default}(x, y, 
      method = "rf",  
      preProcess = NULL,
      ..., 
      weights = NULL,
      metric = ifelse(is.factor(y), "Accuracy", "RMSE"),   
      maximize = ifelse(metric == "RMSE", FALSE, TRUE),
      trControl = trainControl(), 
      tuneGrid = NULL, 
      tuneLength = 3)

\method{train}{formula}(form, data, ..., weights, subset, na.action, contrasts = NULL)
}
\arguments{
  \item{x}{a data frame containing training data where samples are in rows and features are in columns.}
  \item{y}{a numeric or factor vector containing the outcome for each sample.}
\item{form}{A formula of the form \code{y ~ x1 + x2 + ...}}
\item{data}{Data frame from which variables specified in \code{formula} are preferentially to be taken.}
  \item{weights}{a numeric vector of case weights. This argument will only affect models that allow case weights.}
\item{subset}{An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.)}
\item{na.action}{A function to specify the action to be taken if NAs are found. The default action is for the procedure to fail. An alternative is na.omit, which leads to rejection of cases with missing values on any required variable. (NOTE: If given, this argument must be named.)}
\item{contrasts}{a list of contrasts to be used for some or all of the factors appearing as variables in the model formula.}
  \item{method}{a string specifying which classification or regression model to use. Possible values are: \code{ada}, \code{avNNet}, \code{bag}, \code{bagEarth}, \code{bagFDA}, \code{blackboost}, \code{Boruta}, \code{bstTree}, \code{bstLs}, \code{bstSm}, \code{cforest}, \code{ctree}, \code{ctree2}, \code{cubist}, \code{earth}, \code{enet}, \code{fda}, \code{foba}, \code{gam}, \code{gamboost}, \code{gamLoess}, \code{gamSpline}, \code{gaussprPoly}, \code{gaussprRadial}, \code{gaussprLinear}, \code{gbm}, \code{gcvEarth}, \code{glm}, \code{glmboost}, \code{glmnet}, \code{gpls}, \code{hda},  \code{hdda}, \code{icr}, \code{J48}, \code{JRip}, \code{knn}, \code{lars}, \code{lasso}, \code{lda},\code{leapForward}, \code{leapBackward}, \code{leapSeq}, \code{Linda}, \code{lm}, \code{lmStepAIC}, \code{LMT}, \code{logforest}, \code{logitBoost}, \code{logreg}, \code{lssvmPoly}, \code{lssvmRadial}, \code{lvq}, \code{M5}, \code{M5Rules}, \code{mda}, \code{multinom}, \code{nb}, \code{neuralnet}, \code{nnet}, \code{nodeHarvest}, \code{OneR}, \code{pam}, \code{parRF}, \code{partDSA}, \code{pcaNNet}, \code{pcr}, \code{pda}, \code{pda2}, \code{penalized}, \code{plr}, \code{pls}, \code{ppr}, \code{qda}, \code{QdaCov}, \code{qrnn}, \code{rda}, \code{rf}, \code{ridge}, \code{rlm}, \code{rocc}, \code{rpart}, \code{rpart2}, \code{rrlda}, \code{rvmLinear}, \code{rvmPoly}, \code{rvmRadial}, \code{sda}, \code{sddaLDA}, \code{sddaQDA}, \code{simpls}, \code{slda}, \code{smda}, \code{sparseLDA}, \code{spls}, \code{stepLDA}, \code{stepQDA}, \code{superpc}, \code{svmPoly}, \code{svmRadial}, \code{svmRadialCost}, \code{svmLinear}, \code{treebag}, \code{vbmpRadial} and \code{widekernelpls}. See the Details section below.}  
  \item{\ldots}{ arguments passed to the classification or regression routine (such as \code{\link[randomForest]{randomForest}}). Errors will occur if values
         for tuning parameters are passed here.} 
\item{preProcess}{a string vector that defines an pre-processing of the predictor data. Current possibilities are \code{center}, \code{scale}, \code{spatialSign}, \code{pca}, \code{ica}, and \code{knnImpute}. See \code{\link{preProcess}} and \code{\link{trainControl}} on the procedures and how to adjust them.} 
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model. By default, possible values are "RMSE" and "Rsquared" for regression and "Accuracy" and "Kappa" for classification. If custom performance metrics are used (via the \code{summaryFunction} argument in \code{\link{trainControl}}, the value of \code{metric} should match one of the arguments. If it does not, a warning is issued and the first metric given by the \code{summaryFunction} is used. (NOTE: If given, this argument must be named.)}         
  \item{maximize}{a logical: should the metric be maximized or minimized?}
  \item{trControl}{a list of values that define how this function acts. See \code{\link{trainControl}}. (NOTE: If given, this argument must be named.)}
  \item{tuneGrid}{a data frame with possible tuning values. The columns are named the same as the tuning parameters in each
         method preceded by a period (e.g. .decay, .lambda). Also, a function can be passed to \code{tuneGrid} with arguments called \code{len} and \code{data}. The output of the function should be the same as the output produced by  \code{\link{createGrid}}.
         (NOTE: If given, this argument must be named.)}  
  \item{tuneLength}{an integer denoting the number of levels for each tuning parameters that should be
         generated by \code{\link{createGrid}}. (NOTE: If given, this argument must be named.)}
}
\details{
\code{train} can be used to tune models by picking the complexity parameters that are associated with the optimal resampling statistics. For particular model, a grid of parameters (if any) is created and the model is trained on slightly different data for each candidate combination of tuning parameters. Across each data set, the performance of held-out samples is calculated and the mean and standard deviation is summarized for each combination. The combination with the optimal resampling statistic is chosen as the final model and the entire training set is used to fit a final model.

A variety of models are currently available. The table below enumerates the models and the values of the \code{method} argument, as well as the complexity parameters used by \code{train}.

  
\tabular{lccc}{
\bold{Model} \tab \bold{\code{method} Value} \tab \bold{Package} \tab \bold{Tuning Parameter(s)} \cr
Generalized linear model \tab \code{glm} \tab \pkg{stats} \tab none \cr
                         \tab \code{glmStepAIC} \tab \pkg{MASS} \tab none \cr
Generalized additive model \tab \code{gam} \tab \pkg{mgcv} \tab \code{select}, \code{method} \cr   
                           \tab \code{gamLoess} \tab \pkg{gam} \tab \code{span}, \code{degree} \cr      
                           \tab \code{gamSpline} \tab \pkg{gam} \tab \code{df} \cr     
Recursive partitioning \tab \code{rpart} \tab \pkg{rpart} \tab \code{cp} \cr  
                       \tab \code{rpart2} \tab \pkg{rpart} \tab \code{maxdepth} \cr  
                       \tab \code{ctree} \tab \pkg{party} \tab \code{mincriterion} \cr   
                       \tab \code{ctree2} \tab \pkg{party} \tab \code{maxdepth} \cr
Boosted trees \tab \code{gbm} \tab \pkg{gbm} \tab \code{interaction depth}, \cr
              \tab \tab \tab \code{n.trees}, \code{shrinkage}  \cr
              \tab \code{blackboost} \tab \pkg{mboost} \tab \code{maxdepth}, \code{mstop}\cr
              \tab \code{ada} \tab \pkg{ada} \tab \code{maxdepth}, \code{iter}, \code{nu}\cr       
              \tab \code{bstTree} \tab \pkg{bst} \tab \code{maxdepth}, \code{mstop}, \code{nu}\cr       
Boosted regression models \tab \code{glmboost} \tab \pkg{mboost} \tab \code{mstop}\cr   
                          \tab \code{gamboost} \tab \pkg{mboost} \tab  \code{mstop}\cr    
                          \tab \code{logitBoost} \tab \pkg{caTools} \tab  \code{nIter}\cr  
                          \tab \code{bstLs} \tab \pkg{bst} \tab \code{mstop}, \code{nu}\cr   
                          \tab \code{bstSm} \tab \pkg{bst} \tab \code{mstop}, \code{nu}\cr  
Random forests \tab  \code{rf} \tab \pkg{randomForest} \tab \code{mtry} \cr
               \tab \code{parRF} \tab \pkg{randomForest}, \pkg{foreach} \tab \code{mtry} \cr   
               \tab \code{cforest} \tab \pkg{party} \tab \code{mtry} \cr   
               \tab \code{Boruta} \tab \pkg{Boruta} \tab \code{mtry} \cr 
Bagging \tab \code{treebag} \tab \pkg{ipred} \tab None \cr 
        \tab \code{bag}     \tab \pkg{caret} \tab \code{vars} \cr
	\tab \code{logicBag}\tab \pkg{logicFS}\tab \code{ntrees}, \code{nleaves}\cr
Other Trees \tab \code{nodeHarvest} \tab \pkg{nodeHarvest} \tab \code{maxinter}, \code{node} \cr 
            \tab \code{partDSA} \tab \pkg{partDSA} \tab \code{cut.off.growth}, \code{MPD} \cr 
Logic Regression \tab \code{logreg} \tab \pkg{LogicReg} \tab \code{ntrees}, \code{treesize} \cr
Elastic net (glm) \tab  \code{glmnet} \tab \pkg{glmnet} \tab \code{alpha}, \code{lambda} \cr
Neural networks \tab  \code{nnet} \tab \pkg{nnet} \tab \code{decay}, \code{size} \cr
                \tab \code{neuralnet} \tab \pkg{neuralnet} \tab \code{layer1}, \code{layer2}, \code{layer3} \cr
                \tab \code{pcaNNet} \tab  \pkg{caret} \tab \code{decay}, \code{size} \cr
                \tab \code{avNNet} \tab  \pkg{caret} \tab \code{decay}, \code{size}, \code{bag} \cr
Projection pursuit regression \tab \code{ppr} \tab \pkg{stats} \tab \code{nterms} \cr
Principal component regression \tab \code{pcr} \tab \pkg{pls} \tab \code{ncomp} \cr
Independent component regression \tab \code{icr} \tab \pkg{caret} \tab \code{n.comp} \cr
Partial least squares \tab \code{pls} \tab \pkg{pls}, \pkg{caret} \tab \code{ncomp} \cr
                      \tab \code{simpls} \tab \pkg{pls}, \pkg{caret} \tab \code{ncomp} \cr
                      \tab \code{widekernelpls} \tab \pkg{pls}, \pkg{caret} \tab \code{ncomp} \cr
Sparse partial least squares \tab \code{spls} \tab \pkg{spls}, \pkg{caret} \tab \code{K}, \code{eta}, \code{kappa} \cr
Support vector machines  \tab \code{svmLinear} \tab \pkg{kernlab} \tab \code{C}\cr 
                         \tab \code{svmRadial} \tab \pkg{kernlab} \tab \code{sigma}, \code{C}\cr 
                         \tab \code{svmRadialCost} \tab \pkg{kernlab} \tab \code{C}\cr 
                         \tab \code{svmPoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}, \code{C}\cr 
Relevance vector machines \tab \code{rvmLinear} \tab \pkg{kernlab} \tab none\cr 
                          \tab \code{rvmRadial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
                          \tab \code{rvmPoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}\cr 
Least squares support vector machines   \tab \code{lssvmRadial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
Gaussian processes  \tab \code{guassprLinearl} \tab \pkg{kernlab} \tab none\cr 
                    \tab \code{guassprRadial} \tab \pkg{kernlab} \tab \code{sigma}\cr 
                    \tab \code{guassprPoly} \tab \pkg{kernlab} \tab \code{scale}, \code{degree}\cr 
Linear least squares \tab \code{lm} \tab \pkg{stats} \tab None \cr 
                     \tab \code{lmStepAIC} \tab \pkg{MASS} \tab None \cr 
                     \tab \code{leapForward} \tab \pkg{leaps} \tab \code{nvmax} \cr 
                     \tab \code{leapBackward} \tab \pkg{leaps} \tab \code{nvmax} \cr 
                     \tab \code{leapSeq} \tab \pkg{leaps} \tab \code{nvmax} \cr 
Robust linear regression \tab \code{rlm} \tab \pkg{MASS} \tab None \cr 
Multivariate adaptive regression splines \tab \code{earth} \tab \pkg{earth} \tab \code{degree}, \code{nprune} \cr
                                         \tab \code{gcvEarth} \tab \pkg{earth} \tab \code{degree} \cr
Bagged MARS \tab \code{bagEarth} \tab \pkg{caret},  \pkg{earth}\tab \code{degree}, \code{nprune} \cr 
Rule Based Regression \tab \code{M5Rules} \tab \pkg{RWeka} \tab \code{pruned}, \code{smoothed} \cr 
                      \tab \code{M5} \tab \pkg{RWeka} \tab \code{pruned}, \code{smoothed}, \code{rules} \cr 
                      \tab \code{cubist} \tab \pkg{Cubist} \tab \code{committees}, \code{neighbors} \cr 
Penalized linear models \tab \code{penalized} \tab \pkg{penalized} \tab \code{lambda1}, \code{lambda2}\cr
                        \tab \code{ridge} \tab \pkg{elasticnet} \tab \code{lambda} \cr
                        \tab \code{enet} \tab \pkg{elasticnet} \tab \code{lambda}, \code{fraction} \cr
                        \tab \code{lars} \tab \pkg{lars} \tab \code{fraction} \cr
                        \tab \code{lars2} \tab \pkg{lars} \tab \code{steps} \cr
                        \tab \code{enet} \tab \pkg{elasticnet} \tab \code{fraction} \cr
                        \tab \code{foba} \tab \pkg{foba} \tab \code{lambda}, \code{k} \cr
Supervised principal components \tab \code{superpc} \tab \pkg{superpc} \tab \code{n.components}, \code{threshold} \cr 
Quantile regression forests \tab \code{qrf} \tab \pkg{quantregForest} \tab \code{mtry} \cr
Quantile regression neural networks \tab \code{qrnn} \tab \pkg{qrnn} \tab \code{n.hidden}, \code{penalty}, \code{bag} \cr 
Linear discriminant analysis \tab \code{lda} \tab \pkg{MASS} \tab  None\cr 
                             \tab \code{Linda} \tab \pkg{rrcov} \tab  None\cr 
Quadratic discriminant analysis \tab \code{qda} \tab \pkg{MASS} \tab  None\cr 
                                \tab \code{QdaCov} \tab \pkg{rrcov} \tab  None\cr 
Stabilized linear discriminant analysis \tab \code{slda} \tab \pkg{ipred} \tab  None\cr 
Heteroscedastic discriminant analysis \tab \code{hda} \tab \pkg{hda} \tab \code{newdim}, \code{lambda}, \code{gamma} \cr
Stepwise discriminant analysis \tab \code{stepLDA} \tab \pkg{klaR} \tab \code{maxvar}, \code{direction} \cr
                               \tab \code{stepQDA} \tab \pkg{klaR} \tab \code{maxvar}, \code{direction} \cr
Stepwise diagonal discriminant analysis \tab \code{sddaLDA} \tab \pkg{SDDA} \tab None \cr
                                        \tab \code{sddaQDA} \tab \pkg{SDDA} \tab None \cr
Shrinkage discriminant analysis \tab \code{sda} \tab \pkg{sda} \tab \code{diagonal} \cr
Sparse linear discriminant analysis \tab \code{sparseLDA} \tab \pkg{sparseLDA} \tab \code{NumVars}, \code{lambda} \cr
Regularized discriminant analysis \tab \code{rda} \tab \pkg{klaR} \tab \code{lambda}, \code{gamma} \cr
Mixture discriminant analysis \tab \code{mda} \tab \pkg{mda} \tab \code{subclasses} \cr
Sparse mixture discriminant analysis \tab \code{smda} \tab \pkg{sparseLDA} \tab \code{NumVars}, \code{R}, \code{lambda} \cr
Penalized discriminant analysis \tab \code{pda} \tab \pkg{mda} \tab \code{lambda}\cr
                                \tab \code{pda2} \tab \pkg{mda} \tab \code{df}\cr
Stabilised linear discriminant analysis \tab \code{slda} \tab \pkg{ipred} \tab None\cr
High dimensional discriminant analysis \tab \code{hdda} \tab \pkg{HDclassif} \tab \code{model}, \code{threshold} \cr
Flexible discriminant analysis (MARS) \tab \code{fda} \tab \pkg{mda}, \pkg{earth} \tab \code{degree}, \code{nprune} \cr
Robust Regularized Linear Discriminant Analysis \tab \code{rrlda} \tab \pkg{rrlda} \tab  \code{lambda}, \code{alpha} \cr 
Bagged FDA \tab \code{bagFDA} \tab \pkg{caret},  \pkg{earth} \tab \code{degree}, \code{nprune} \cr  
Logistic/multinomial regression \tab \code{multinom} \tab \pkg{nnet} \tab \code{decay}\cr 
Penalized logistic regression \tab \code{plr} \tab \pkg{stepPlr} \tab \code{lambda}, \code{cp}\cr 
Rule--based classification \tab \code{J48} \tab \pkg{RWeka} \tab \code{C}\cr 
                           \tab \code{OneR} \tab \pkg{RWeka} \tab None\cr
                           \tab \code{PART} \tab \pkg{RWeka} \tab \code{threshold}, \code{pruned}\cr
                           \tab \code{JRip} \tab \pkg{RWeka} \tab \code{NumOpt}\cr
Logic Forests \tab \code{logforest} \tab \pkg{LogicForest} \tab None \cr
Bayesian multinomial probit model \tab \code{vbmpRadial} \tab \pkg{vbmp} \tab \code{estimateTheta} \cr
k nearest neighbors \tab \code{knn3} \tab \pkg{caret} \tab \code{k} \cr 
Nearest shrunken centroids \tab \code{pam} \tab \pkg{pamr} \tab \code{threshold} \cr  
                           \tab \code{scrda} \tab \pkg{rda} \tab \code{alpha}, \code{delta} \cr 
Naive Bayes \tab \code{nb} \tab \pkg{klaR} \tab \code{usekernel}, \code{fL} \cr
Generalized partial least squares\tab \code{gpls} \tab \pkg{gpls} \tab \code{K.prov} \cr
Learned vector quantization \tab \code{lvq} \tab \pkg{class} \tab  \code{size}, \code{k} \cr
ROC Curves \tab \code{rocc} \tab \code{rocc} \tab \code{xgenes} 
}


By default, the function \code{\link{createGrid}} is used to define the candidate values of the tuning parameters. The user can also specify their own. To do this, a data fame is created with columns for each tuning parameter in the model. The column names must be the same as those listed in the table above with a leading dot. For example, \code{ncomp} would have the column heading \code{.ncomp}. This data frame can then be passed to \code{\link{createGrid}}.

In some cases, models may require control arguments. These can be passed via the three dots argument. Note that some models can specify tuning parameters in the control objects. If specified, these values will be superseded by those given in the \code{\link{createGrid}} argument. 

The vignette entitled "caret Manual -- Model Building" has more details and examples related to this function.

\code{train} can be used with "explicit parallelism", where different resamples (e.g. cross-validation group) and models can be split up and run on multiple machines or processors. By default, \code{train} will use a single processor on the host machine. As of version 4.99 of this package, the framework used for parallel processing uses the \pkg{foreach} package. To run the resamples in parallel, the code for \code{train} does not change; prior to the call to \code{train}, a parallel backend is registered with \pkg{foreach} (see the examples below). 
}



\value{
  A list is returned of class \code{train} containing:
  \item{method }{the chosen model.}  
  \item{modelType }{an identifier of the model type.}  
  \item{results }{a data frame the training error rate and values of the tuning parameters.}
  \item{bestTune }{a data frame with the final parameters.}  

  \item{call }{the (matched) function call with dots expanded}
  \item{dots}{a list containing any ... values passed to the original call}
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model.}  
  \item{control}{the list of control parameters.}
  \item{preProcess }{either \code{NULL} or an object of class \code{\link{preProcess}}}
   \item{finalModel}{an fit object using the best parameters}
   \item{trainingData}{a data frame}
   \item{resample}{A data frame with columns for each performance
     metric. Each row corresponds to each resample. If leave-one-out
     cross-validation or out-of-bag estimation methods are requested,
     this will be \code{NULL}. The \code{returnResamp} argument of \code{\link{trainControl}}
     controls how much of the resampled results are saved.}
  \item{perfNames}{a character vector of performance metrics that are produced by the summary function}
  \item{maximize}{a logical recycled  from the function arguments.}
  \item{yLimits}{the range of the training set outcomes.}
  \item{times}{a list of execution times: \code{everything} is for the entire call to \code{train}, \code{final} for the final model fit and, optionally, \code{prediction} for the time to predict new samples (see \code{\link{trainControl}})}
}

\author{Max Kuhn (the guts of \code{train.formula} were based on Ripley's \code{nnet.formula})}

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{\code{\link{trainControl}}, \code{\link{createGrid}}, \code{\link{createFolds}}}
\examples{

#######################################
## Classification Example

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

knnFit2 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10, 
                 trControl = trainControl(method = "boot"))


library(MASS)
nnetFit <- train(TrainData, TrainClasses,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

#######################################
## Regression Example

library(mlbench)
data(BostonHousing)

lmFit <- train(medv ~ . + rm:lstat,
               data = BostonHousing, 
               "lm")

library(rpart)
rpartFit <- train(medv ~ .,
                  data = BostonHousing,
                  "rpart",
                  tuneLength = 9)

#######################################
## Example with a custom metric

madSummary <- function (data,
                        lev = NULL,
                        model = NULL) 
{
  out <- mad(data$obs - data$pred, 
             na.rm = TRUE)  
  names(out) <- "MAD"
  out
}

robustControl <- trainControl(summaryFunction = madSummary)
marsGrid <- expand.grid(.degree = 1,
                        .nprune = (1:10) * 2)

earthFit <- train(medv ~ .,
                  data = BostonHousing, 
                  "earth",
                  tuneGrid = marsGrid,
                  metric = "MAD",
                  maximize = FALSE,
                  trControl = robustControl)

#######################################
## Parallel Processing Example via multicore package

\dontrun{

library(doMC)
registerDoMC(2)

## NOTE: don't run models form RWeka when using
### multicore. The session will crash.

## The code for train() does not change:
set.seed(1)
usingMC <-  train(medv ~ .,
                  data = BostonHousing, 
                  "glmboost")

## or use:
## library(doMPI) or 
## library(doSMP) and so on

}

}
\keyword{models}
