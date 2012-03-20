\name{train}
\alias{train}
\alias{train.default}
\alias{train.formula}

\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\bpkg}{\href{http://www.bioconductor.org/packages/release/bioc/html/#1.html}{\pkg{#1}}}

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
  \item{method}{a string specifying which classification or regression model to use. Possible values are: \code{ada}, \code{avNNet}, \code{bag}, \code{bagEarth}, \code{bagFDA}, \code{bdk}, \code{blackboost}, \code{Boruta}, \code{bstLs}, \code{bstSm}, \code{bstTree}, \code{cforest}, \code{ctree}, \code{ctree2}, \code{cubist}, \code{earth}, \code{earthTest}, \code{enet}, \code{evtree}, \code{fda}, \code{foba}, \code{gam}, \code{gamboost}, \code{GAMens}, \code{gamLoess}, \code{gamSpline}, \code{gaussprLinear}, \code{gaussprPoly}, \code{gaussprRadial}, \code{gbm}, \code{gcvEarth}, \code{glm}, \code{glmboost}, \code{glmnet}, \code{glmStepAIC}, \code{gpls}, \code{hda}, \code{hdda}, \code{icr}, \code{J48}, \code{JRip}, \code{kernelpls}, \code{knn}, \code{krlsPoly}, \code{krlsRadial}, \code{lars}, \code{lars2}, \code{lasso}, \code{lda}, \code{lda2}, \code{leapBackward}, \code{leapForward}, \code{leapSeq}, \code{Linda}, \code{lm}, \code{lmStepAIC}, \code{LMT}, \code{logforest}, \code{logicBag}, \code{logitBoost}, \code{logreg}, \code{lssvmLinear}, \code{lssvmPoly}, \code{lssvmRadial}, \code{lvq}, \code{M5}, \code{M5Rules}, \code{mars}, \code{mda}, \code{mlp}, \code{mlpWeightDecay}, \code{multinom}, \code{nb}, \code{neuralnet}, \code{nnet}, \code{nodeHarvest}, \code{obliqueTree}, \code{OneR}, \code{ORFlog}, \code{ORFpls}, \code{ORFridge}, \code{ORFsvm}, \code{pam}, \code{parRF}, \code{PART}, \code{partDSA}, \code{pcaNNet}, \code{pcr}, \code{pda}, \code{pda2}, \code{penalized}, \code{PenalizedLDA}, \code{plr}, \code{pls}, \code{plsTest}, \code{ppr}, \code{qda}, \code{QdaCov}, \code{qrf}, \code{qrnn}, \code{rbf}, \code{rbfDDA}, \code{rda}, \code{relaxo}, \code{rf}, \code{rFerns}, \code{rfLSF}, \code{rfNWS}, \code{ridge}, \code{rlm}, \code{rocc}, \code{rpart}, \code{rpart2}, \code{RRF}, \code{RRFglobal}, \code{rrlda}, \code{rvmLinear}, \code{rvmPoly}, \code{rvmRadial}, \code{scrda}, \code{sda}, \code{sddaLDA}, \code{sddaQDA}, \code{simpls}, \code{slda}, \code{smda}, \code{sparseLDA}, \code{spls}, \code{stepLDA}, \code{stepQDA}, \code{superpc}, \code{svmLinear}, \code{svmpoly}, \code{svmPoly}, \code{svmradial}, \code{svmRadial}, \code{svmRadialCost}, \code{treebag}, \code{vbmpRadial}, \code{widekernelpls}, \code{xyf}. See the Details section below.}  
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

A variety of models are currently available. The lists below enumerate the models and the values of the \code{method} argument, as well as the complexity parameters used by \code{train}.

\bold{ Bagging }
\itemize{
  \item \emph{Method Value}: \code{bag} from package \pkg{caret} with tuning parameter \code{vars} (dual use) 
  \item \emph{Method Value}: \code{bagEarth} from package \pkg{caret} with tuning parameters: \code{degree}, \code{nprune} (dual use) 
  \item \emph{Method Value}: \code{bagFDA} from package \pkg{caret} with tuning parameters: \code{nprune}, \code{degree} (classification only) 
  \item \emph{Method Value}: \code{GAMens} from package \cpkg{GAMens} with tuning parameters: \code{fusion}, \code{rsm_size}, \code{iter} (classification only) 
  \item \emph{Method Value}: \code{logicBag} from package \bpkg{logicFS} with tuning parameters: \code{nleaves}, \code{ntrees} (dual use) 
  \item \emph{Method Value}: \code{treebag} from package \cpkg{ipred}  with no tuning parameters (dual use) 
}

\bold{ Bayesian Methods }
\itemize{
  \item \emph{Method Value}: \code{nb} from package \cpkg{klaR} with tuning parameters: \code{usekernel}, \code{fL} (classification only) 
  \item \emph{Method Value}: \code{vbmpRadial} from package \bpkg{vbmp} with tuning parameter \code{estimateTheta} (classification only) 
}

\bold{ Boosted Trees }
\itemize{
  \item \emph{Method Value}: \code{ada} from package \cpkg{ada} with tuning parameters: \code{iter}, \code{maxdepth}, \code{nu} (dual use) 
  \item \emph{Method Value}: \code{blackboost} from package \cpkg{mboost} with tuning parameters: \code{mstop}, \code{maxdepth} (dual use) 
  \item \emph{Method Value}: \code{bstTree} from package \cpkg{bst} with tuning parameters: \code{nu}, \code{maxdepth}, \code{mstop} (dual use) 
  \item \emph{Method Value}: \code{gbm} from package \cpkg{gbm} with tuning parameters: \code{shrinkage}, \code{n.trees}, \code{interaction.depth} (dual use) 
}

\bold{ Boosting (Non-Tree) }
\itemize{
  \item \emph{Method Value}: \code{bstLs} from package \cpkg{bst} with tuning parameters: \code{mstop}, \code{nu} (dual use) 
  \item \emph{Method Value}: \code{bstSm} from package \cpkg{bst} with tuning parameters: \code{mstop}, \code{nu} (dual use) 
  \item \emph{Method Value}: \code{gamboost} from package \cpkg{mboost} with tuning parameters: \code{mstop}, \code{prune} (dual use) 
  \item \emph{Method Value}: \code{glmboost} from package \cpkg{mboost} with tuning parameters: \code{prune}, \code{mstop} (dual use) 
  \item \emph{Method Value}: \code{logitBoost} from package \cpkg{caTools} with tuning parameter \code{nIter} (classification only) 
}

\bold{ Elastic Net }
\itemize{
  \item \emph{Method Value}: \code{glmnet} from package \cpkg{glmnet} with tuning parameters: \code{alpha}, \code{lambda} (dual use) 
}

\bold{ Flexible Discriminant Analysis (MARS basis) }
\itemize{
  \item \emph{Method Value}: \code{fda} from package \cpkg{mda} with tuning parameters: \code{nprune}, \code{degree} (classification only) 
}

\bold{ Gaussian Processes }
\itemize{
  \item \emph{Method Value}: \code{gaussprLinear} from package \cpkg{kernlab}  with no tuning parameters (dual use) 
  \item \emph{Method Value}: \code{gaussprPoly} from package \cpkg{kernlab} with tuning parameters: \code{degree}, \code{scale} (dual use) 
  \item \emph{Method Value}: \code{gaussprRadial} from package \cpkg{kernlab} with tuning parameter \code{sigma} (dual use) 
}

\bold{ Generalized additive model }
\itemize{
  \item \emph{Method Value}: \code{gam} from package \cpkg{mgcv} with tuning parameters: \code{select}, \code{method} (dual use) 
  \item \emph{Method Value}: \code{gamLoess} from package \cpkg{gam} with tuning parameters: \code{degree}, \code{span} (dual use) 
  \item \emph{Method Value}: \code{gamSpline} from package \cpkg{gam} with tuning parameter \code{df} (dual use) 
}

\bold{ Generalized linear model }
\itemize{
  \item \emph{Method Value}: \code{glm} from package \pkg{stats}  with no tuning parameters (dual use) 
  \item \emph{Method Value}: \code{glmStepAIC} from package \cpkg{MASS}  with no tuning parameters (dual use) 
}

\bold{ Heteroscedastic Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{hda} from package \cpkg{hda} with tuning parameters: \code{lambda}, \code{gamma}, \code{newdim} (classification only) 
}

\bold{ High Dimensional Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{hdda} from package \cpkg{HDclassif} with tuning parameters: \code{threshold}, \code{model} (classification only) 
}

\bold{ Independent Component Regression }
\itemize{
  \item \emph{Method Value}: \code{icr} from package \pkg{caret} with tuning parameter \code{n.comp} (regression only) 
}

\bold{ K Nearest Neighbor }
\itemize{
  \item \emph{Method Value}: \code{knn} from package \pkg{caret} with tuning parameter \code{k} (dual use) 
}

\bold{ Learned Vector  Quantization }
\itemize{
  \item \emph{Method Value}: \code{lvq} from package \cpkg{class} with tuning parameters: \code{size}, \code{k} (classification only) 
}

\bold{ Linear Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{lda} from package \cpkg{MASS}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{lda2} from package \cpkg{MASS} with tuning parameter \code{dimen} (classification only) 
  \item \emph{Method Value}: \code{Linda} from package \cpkg{rrcov}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{rrlda} from package \cpkg{rrlda} with tuning parameters: \code{alpha}, \code{lambda} (classification only) 
  \item \emph{Method Value}: \code{sda} from package \cpkg{sda} with tuning parameter \code{diagonal} (classification only) 
  \item \emph{Method Value}: \code{sddaLDA} from package \cpkg{SDDA}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{slda} from package \cpkg{ipred}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{stepLDA} from package \cpkg{klaR} with tuning parameters: \code{direction}, \code{maxvar} (classification only) 
}

\bold{ Linear Least Squares }
\itemize{
  \item \emph{Method Value}: \code{leapBackward} from package \cpkg{leaps} with tuning parameter \code{nvmax} (regression only) 
  \item \emph{Method Value}: \code{leapForward} from package \cpkg{leaps} with tuning parameter \code{nvmax} (regression only) 
  \item \emph{Method Value}: \code{leapSeq} from package \cpkg{leaps} with tuning parameter \code{nvmax} (regression only) 
  \item \emph{Method Value}: \code{lm} from package \pkg{stats}  with no tuning parameters (regression only) 
  \item \emph{Method Value}: \code{lmStepAIC} from package \cpkg{MASS}  with no tuning parameters (regression only) 
  \item \emph{Method Value}: \code{rlm} from package \cpkg{MASS}  with no tuning parameters (regression only) 
}

\bold{ Logic Regression }
\itemize{
  \item \emph{Method Value}: \code{logforest} from package \cpkg{LogForest}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{logreg} from package \cpkg{LogicReg} with tuning parameters: \code{treesize}, \code{ntrees} (dual use) 
}

\bold{ Logistic Model Trees }
\itemize{
  \item \emph{Method Value}: \code{LMT} from package \cpkg{RWeka} with tuning parameter \code{iter} (classification only) 
}

\bold{ Logistic/Multinomial Regression }
\itemize{
  \item \emph{Method Value}: \code{multinom} from package \cpkg{nnet} with tuning parameter \code{decay} (classification only) 
  \item \emph{Method Value}: \code{plr} from package \cpkg{stepPlr} with tuning parameters: \code{cp}, \code{lambda} (classification only) 
}

\bold{ Mixture Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{mda} from package \cpkg{mda} with tuning parameter \code{subclasses} (classification only) 
  \item \emph{Method Value}: \code{smda} from package \cpkg{sparseLDA} with tuning parameters: \code{R}, \code{lambda}, \code{NumVars} (classification only) 
}

\bold{ Multivariate Adaptive Regression Spline }
\itemize{
  \item \emph{Method Value}: \code{earth} from package \cpkg{earth} with tuning parameters: \code{degree}, \code{nprune} (dual use) 
  \item \emph{Method Value}: \code{gcvEarth} from package \cpkg{earth} with tuning parameter \code{degree} (dual use) 
}

\bold{ Nearest Shrunken Centroids }
\itemize{
  \item \emph{Method Value}: \code{pam} from package \cpkg{pamr} with tuning parameter \code{threshold} (classification only) 
  \item \emph{Method Value}: \code{scrda} from package \cpkg{rda} with tuning parameters: \code{alpha}, \code{delta} (classification only) 
}

\bold{ Neural Networks }
\itemize{
  \item \emph{Method Value}: \code{avNNet} from package \pkg{caret} with tuning parameters: \code{decay}, \code{bag}, \code{size} (dual use) 
  \item \emph{Method Value}: \code{mlp} from package \cpkg{RSNNS} with tuning parameter \code{size} (dual use) 
  \item \emph{Method Value}: \code{mlpWeightDecay} from package \cpkg{RSNNS} with tuning parameters: \code{size}, \code{decay} (dual use) 
  \item \emph{Method Value}: \code{neuralnet} from package \cpkg{neuralnet} with tuning parameters: \code{layer1}, \code{layer3}, \code{layer2} (regression only) 
  \item \emph{Method Value}: \code{nnet} from package \cpkg{nnet} with tuning parameters: \code{decay}, \code{size} (dual use) 
  \item \emph{Method Value}: \code{pcaNNet} from package \pkg{caret} with tuning parameters: \code{decay}, \code{size} (dual use) 
  \item \emph{Method Value}: \code{qrnn} from package \cpkg{qrnn} with tuning parameters: \code{penalty}, \code{bag}, \code{n.hidden} (regression only) 
}

\bold{ Partial Least Squares }
\itemize{
  \item \emph{Method Value}: \code{gpls} from package \bpkg{gpls} with tuning parameter \code{K.prov} (classification only) 
  \item \emph{Method Value}: \code{kernelpls} from package \cpkg{pls} with tuning parameter \code{ncomp} (dual use) 
  \item \emph{Method Value}: \code{pls} from package \cpkg{pls} with tuning parameter \code{ncomp} (dual use) 
  \item \emph{Method Value}: \code{simpls} from package \cpkg{pls} with tuning parameter \code{ncomp} (dual use) 
  \item \emph{Method Value}: \code{spls} from package \cpkg{spls} with tuning parameters: \code{eta}, \code{kappa}, \code{K} (dual use) 
  \item \emph{Method Value}: \code{widekernelpls} from package \cpkg{pls} with tuning parameter \code{ncomp} (dual use) 
}

\bold{ Penalized Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{pda} from package \cpkg{mda} with tuning parameter \code{lambda} (classification only) 
  \item \emph{Method Value}: \code{pda2} from package \cpkg{mda} with tuning parameter \code{df} (classification only) 
}

\bold{ Penalized Linear Models }
\itemize{
  \item \emph{Method Value}: \code{enet} from package \cpkg{elasticnet} with tuning parameters: \code{fraction}, \code{lambda} (regression only) 
  \item \emph{Method Value}: \code{foba} from package \cpkg{foba} with tuning parameters: \code{lambda}, \code{k} (regression only) 
  \item \emph{Method Value}: \code{krlsPoly} from package \cpkg{KRLS} with tuning parameters: \code{degree}, \code{lambda} (regression only) 
  \item \emph{Method Value}: \code{krlsRadial} from package \cpkg{KRLS} with tuning parameters: \code{sigma}, \code{lambda} (regression only) 
  \item \emph{Method Value}: \code{lars} from package \cpkg{lars} with tuning parameter \code{fraction} (regression only) 
  \item \emph{Method Value}: \code{lars2} from package \cpkg{lars} with tuning parameter \code{step} (regression only) 
  \item \emph{Method Value}: \code{lasso} from package \cpkg{elasticnet} with tuning parameter \code{fraction} (regression only) 
  \item \emph{Method Value}: \code{penalized} from package \cpkg{penalized} with tuning parameters: \code{lambda2}, \code{lambda1} (regression only) 
  \item \emph{Method Value}: \code{relaxo} from package \cpkg{relaxo} with tuning parameters: \code{lambda}, \code{phi} (regression only) 
  \item \emph{Method Value}: \code{ridge} from package \cpkg{elasticnet} with tuning parameter \code{lambda} (regression only) 
}

\bold{ Principal Component Regression }
\itemize{
  \item \emph{Method Value}: \code{pcr} from package \cpkg{pls} with tuning parameter \code{ncomp} (regression only) 
}

\bold{ Projection Pursuit Regression }
\itemize{
  \item \emph{Method Value}: \code{ppr} from package \pkg{stats} with tuning parameter \code{nterms} (regression only) 
}

\bold{ Quadratic Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{qda} from package \cpkg{MASS}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{QdaCov} from package \cpkg{rrcov}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{sddaQDA} from package \cpkg{SDDA}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{stepQDA} from package \cpkg{klaR} with tuning parameters: \code{maxvar}, \code{direction} (classification only) 
}

\bold{ Radial Basis Function Networks }
\itemize{
  \item \emph{Method Value}: \code{rbf} from package \cpkg{RSNNS} with tuning parameter \code{size} (dual use) 
  \item \emph{Method Value}: \code{rbfDDA} from package \cpkg{RSNNS} with tuning parameter \code{negativeThreshold} (classification only) 
}

\bold{ Random Forests }
\itemize{
  \item \emph{Method Value}: \code{Boruta} from package \cpkg{Boruta} with tuning parameter \code{mtry} (dual use) 
  \item \emph{Method Value}: \code{cforest} from package \cpkg{party} with tuning parameter \code{mtry} (dual use) 
  \item \emph{Method Value}: \code{ORFlog} from package \cpkg{obliqueRF} with tuning parameter \code{mtry} (classification only) 
  \item \emph{Method Value}: \code{ORFpls} from package \cpkg{obliqueRF} with tuning parameter \code{mtry} (classification only) 
  \item \emph{Method Value}: \code{ORFridge} from package \cpkg{obliqueRF} with tuning parameter \code{mtry} (classification only) 
  \item \emph{Method Value}: \code{ORFsvm} from package \cpkg{obliqueRF} with tuning parameter \code{mtry} (classification only) 
  \item \emph{Method Value}: \code{parRF} from package \cpkg{randomForest} with tuning parameter \code{mtry} (dual use) 
  \item \emph{Method Value}: \code{qrf} from package \cpkg{quantregForest} with tuning parameter \code{mtry} (regression only) 
  \item \emph{Method Value}: \code{rf} from package \cpkg{randomForest} with tuning parameter \code{mtry} (dual use) 
  \item \emph{Method Value}: \code{rFerns} from package \cpkg{rFerns} with tuning parameter \code{depth} (classification only) 
  \item \emph{Method Value}: \code{RRF} from package \cpkg{RRF} with tuning parameters: \code{coefImp}, \code{mtry}, \code{coefReg} (dual use) 
  \item \emph{Method Value}: \code{RRFglobal} from package \cpkg{RRF} with tuning parameters: \code{mtry}, \code{coefReg} (dual use) 
}

\bold{ Recursive Partitioning }
\itemize{
  \item \emph{Method Value}: \code{ctree} from package \cpkg{party} with tuning parameter \code{mincriterion} (dual use) 
  \item \emph{Method Value}: \code{ctree2} from package \cpkg{party} with tuning parameter \code{maxdepth} (dual use) 
  \item \emph{Method Value}: \code{evtree} from package \cpkg{evtree} with tuning parameter \code{alpha} (dual use) 
  \item \emph{Method Value}: \code{nodeHarvest} from package \cpkg{nodeHarvest} with tuning parameters: \code{maxinter}, \code{mode} (dual use) 
  \item \emph{Method Value}: \code{obliqueTree} from package \cpkg{obliqueTree} with tuning parameters: \code{oblique.splits}, \code{variable.selection} (dual use) 
  \item \emph{Method Value}: \code{partDSA} from package \cpkg{partDSA} with tuning parameters: \code{cut.off.growth}, \code{MPD} (dual use) 
  \item \emph{Method Value}: \code{rpart} from package \cpkg{rpart} with tuning parameter \code{cp} (dual use) 
  \item \emph{Method Value}: \code{rpart2} from package \cpkg{rpart} with tuning parameter \code{maxdepth} (dual use) 
}

\bold{ Regularized Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{rda} from package \cpkg{klaR} with tuning parameters: \code{lambda}, \code{gamma} (classification only) 
}

\bold{ Relevance Vector Machines }
\itemize{
  \item \emph{Method Value}: \code{rvmLinear} from package \cpkg{kernlab}  with no tuning parameters (regression only) 
  \item \emph{Method Value}: \code{rvmPoly} from package \cpkg{kernlab} with tuning parameters: \code{scale}, \code{degree} (regression only) 
  \item \emph{Method Value}: \code{rvmRadial} from package \cpkg{kernlab} with tuning parameter \code{sigma} (regression only) 
}

\bold{ ROC Curves }
\itemize{
  \item \emph{Method Value}: \code{rocc} from package \cpkg{rocc} with tuning parameter \code{xgenes} (classification only) 
}

\bold{ Rule-Based Models }
\itemize{
  \item \emph{Method Value}: \code{cubist} from package \cpkg{Cubist} with tuning parameters: \code{neighbors}, \code{committees} (regression only) 
  \item \emph{Method Value}: \code{J48} from package \cpkg{RWeka} with tuning parameter \code{C} (classification only) 
  \item \emph{Method Value}: \code{JRip} from package \cpkg{RWeka} with tuning parameter \code{NumOpt} (classification only) 
  \item \emph{Method Value}: \code{M5} from package \cpkg{RWeka} with tuning parameters: \code{pruned}, \code{smoothed}, \code{rules} (regression only) 
  \item \emph{Method Value}: \code{M5Rules} from package \cpkg{RWeka} with tuning parameters: \code{smoothed}, \code{pruned} (regression only) 
  \item \emph{Method Value}: \code{OneR} from package \cpkg{RWeka}  with no tuning parameters (classification only) 
  \item \emph{Method Value}: \code{PART} from package \cpkg{RWeka} with tuning parameters: \code{pruned}, \code{threshold} (classification only) 
}

\bold{ Self-Organizing Maps }
\itemize{
  \item \emph{Method Value}: \code{bdk} from package \cpkg{kohonen} with tuning parameters: \code{topo}, \code{xweight}, \code{xdim}, \code{ydim} (dual use) 
  \item \emph{Method Value}: \code{xyf} from package \cpkg{kohonen} with tuning parameters: \code{xdim}, \code{ydim}, \code{topo}, \code{xweight} (dual use) 
}

\bold{ Sparse Linear Discriminant Analysis }
\itemize{
  \item \emph{Method Value}: \code{PenalizedLDA} from package \cpkg{penalizedLDA} with tuning parameters: \code{lambda}, \code{K} (classification only) 
  \item \emph{Method Value}: \code{sparseLDA} from package \cpkg{sparseLDA} with tuning parameters: \code{lambda}, \code{NumVars} (classification only) 
}

\bold{ Supervised Principal Components }
\itemize{
  \item \emph{Method Value}: \code{superpc} from package \cpkg{superpc} with tuning parameters: \code{threshold}, \code{n.components} (regression only) 
}

\bold{ Support Vector Machines }
\itemize{
  \item \emph{Method Value}: \code{lssvmRadial} from package \cpkg{kernlab} with tuning parameter \code{sigma} (classification only) 
  \item \emph{Method Value}: \code{svmLinear} from package \cpkg{kernlab} with tuning parameter \code{C} (dual use) 
  \item \emph{Method Value}: \code{svmPoly} from package \cpkg{kernlab} with tuning parameters: \code{degree}, \code{scale}, \code{C} (dual use) 
  \item \emph{Method Value}: \code{svmRadial} from package \cpkg{kernlab} with tuning parameters: \code{C}, \code{sigma} (dual use) 
  \item \emph{Method Value}: \code{svmRadialCost} from package \cpkg{kernlab} with tuning parameter \code{C} (dual use) 
}



By default, the function \code{\link{createGrid}} is used to define the candidate values of the tuning parameters. The user can also specify their own. To do this, a data fame is created with columns for each tuning parameter in the model. The column names must be the same as those listed in the table above with a leading dot. For example, \code{ncomp} would have the column heading \code{.ncomp}. This data frame can then be passed to \code{\link{createGrid}}.

In some cases, models may require control arguments. These can be passed via the three dots argument. Note that some models can specify tuning parameters in the control objects. If specified, these values will be superseded by those given in the \code{\link{createGrid}} argument. 

For several models (\code{rpart}, \code{rf}, \code{gbm}, \code{treebag}, \code{nb}, \code{J48}, \code{PART}, \code{JRip}, \code{OneR}, \code{ctree}, \code{cforest} and \code{custom}) factor predictors variables can be passed directly to the underlying modeling function using the interface \code{train(x, y)}. In these cases, it is possible for the models to treat factor variables in a manner different than most (i.e. not as a decomposed set of dummy variables). The formula interface to \code{train} will always convert the factors to dummy variables.

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

\seealso{\code{\link{trainControl}}, \code{\link{update.train}},
\code{\link{modelLookup}}, \code{\link{createGrid}},
\code{\link{createFolds}}}

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
