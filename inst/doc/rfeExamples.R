library(caret)
library(mlbench)

data(BostonHousing)

xData <- model.matrix(medv ~ (crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat)^2,
                     data = BostonHousing)[,-1]
xData <- as.data.frame(scale(xData))
colnames(xData) <- gsub(":", ".", colnames(xData), fixed = TRUE)


set.seed(1)
splits <- createDataPartition(BostonHousing$medv,
                               p = 2/3,
                               times = 30)

set.seed(2)
larsFit <- train(xData, BostonHousing$medv,
                 "lars",
                 tuneLength = 15,
                 trControl = trainControl(
                   method = "LGOCV",
                   index = splits))



######################################################################
######################################################################

set.seed(1)
lmProfile <- rfe(xData, BostonHousing$medv,
                 sizes = (1:8) * 10,
                 rfeControl = rfeControl(
                   functions = lmFuncs, 
                   method = "LGOCV",
                   verbose = FALSE,
                   returnResamp = "final",
                   index = splits))
set.seed(1)
rfProfile <- rfe(xData, BostonHousing$medv,
                 sizes = (1:8) * 10,
                 rfeControl = rfeControl(
                   functions = rfFuncs, 
                   method = "LGOCV",
                   verbose = FALSE,
                   returnResamp = "final",
                   index = splits))

######################################################################
######################################################################

rfFuncs2 <- function()
  {
    list(
         fit = function(x, y, ...)
         {
           library(randomForest)
           randomForest(x, y, ntree = 2000, importance = TRUE, ...)
         },
         pred = function(object, x)
         {
           predict(object, x)
         },
         rank = function(object, x, y)
         {
           vimp <- varImp(object)

           if(is.factor(y))
             {
               if(all(levels(y) %in% colnames(vimp)))
                 {
                   avImp <- apply(vimp[, levels(y), drop = TRUE],
                                  1,
                                  mean)
                   vimp$Overall <- avImp
                 }

             }
           
           vimp <- vimp[
                        order(
                              vimp$Overall,
                              decreasing = TRUE)
                        ,,
                        drop = FALSE]
           
           vimp$var <- rownames(vimp)                  
           vimp
         },
         selectSize = function (x, metric, tol = 3, maximize) 
         {
           index <- 1:nrow(x)
           if (!maximize) {
             best <- min(x[, metric])
             perf <- (x[, metric] - best)/best * 100
             delta <- c(diff(-x[, metric]), 0)
           }
           else {
             best <- max(x[, metric])
             perf <- (x[, metric] - best)/best * -100
             delta <- c(diff(x[, metric]), 0)
           }
           flag <- delta <= 0 & perf <= tol
           if (!any(flag)) 
             flag <- perf <= tol
           candidates <- index[flag]
           bestIter <- min(candidates)
           bestIter
         },
         selectVar = pickVars)
  }


set.seed(1)
rfProfile <- rfe(x, logBBB,
                 sizes = seq(2, 60, by = 2),
                 rfeControl = rfeControl(functions = rfFuncs2(), 
                   number = 100))
set.seed(1)
rfProfile2 <- rfe(x, logBBB,
                  sizes = seq(2, 60, by = 2),
                  rfeControl = rfeControl(functions = rfFuncs2(), 
                    rerank = TRUE, 
                    number = 100))

######################################################################
######################################################################
 
lmProfile$results$Group  <- "OLS"
lmProfile2$results$Group <- "OLS, Re-ranked"
rfProfile$results$Group  <- "RandomForest"
rfProfile2$results$Group <- "RandomForest, Re-ranked"

all <- rbind(lmProfile$results,
             lmProfile2$results,
             rfProfile$results,
             rfProfile2$results)


xyplot(RMSE ~ Variables,
       data = all,
       groups = Group,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"), 
       auto.key = list(columns = 2))


xyplot(Rsquared ~ Variables,
       data = all,
       groups = Group,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"),
       ylab = expression(R^2),
       auto.key = list(columns = 2))

######################################################################
######################################################################

caret:::pickSizeBest(rfProfile$results, "RMSE", FALSE)
caret:::pickSizeTolerance(rfProfile$results,
                          metric = "RMSE",
                          maximize = FALSE)

######################################################################
######################################################################

tolerData <- rfProfile$results
tolerData$RMSE <- (min(tolerData$RMSE) -tolerData$RMSE)/
  min(tolerData$RMSE) * 100

tolerData$Rsquared <- (tolerData$Rsquared - max(tolerData$Rsquared))/
  max(tolerData$Rsquared) * 100

xyplot(RMSETol ~ Variables,
       data = rfProfile$results,
       lmProfile$results$Variables, 
       type = c("g", "p", "l"),
       main = paste("RandomForest (Single Ranking)\nTolerance =",
         expression(tolerance = 100 *(best - RMSE)/best)),
       ylab = "RMSE Tolerance"
       )

trellis.par.set(caretTheme())
plot2 <- xyplot(RMSETol ~ Variables,
                data = rfProfile$results,
                lmProfile$results$Variables, 
                type = c("g", "p", "l"),
                 ylab =  expression(100 *(best - RMSE)/best)),
                ylab = "RMSE Tolerance"
                main = "(b)"
                )

xyplot(RMSE ~ Variables,
       data = tolerData,
       type = c("g", "p", "l"),
       main ="(b)",
       ylab =  expression(100 *(best - RMSE)/best))
       )

######################################################################
######################################################################

set.seed(1)
enetFit <- train(x, logBBB, "enet",
                 tuneGrid = expand.grid(.lambda = c(0, .001, .01, .1),
                   .fraction = (1:20)/20),
                 trControl = trainControl(method = "LGOCV", p =.8, number = 100))

set.seed(1)
enetFit2 <- train(x, logBBB, "enet",
                 tuneGrid = expand.grid(.lambda = .01, .fraction = .2),
                 trControl = trainControl(method = "LGOCV", p =.8, number = 100))

enetCoef <- predict(enetFit2$finalModel,
                    x,
                    s = .2,
                    mode = "fraction",
                    type = "coefficients")$coefficients
enetCoef <- enetCoef[enetCoef != 0]
enetCorr <- cor(x[, colnames(x) %in% names(enetCoef)])
summary(enetCorr[upper.tri(enetCorr)])

######################################################################
######################################################################

set.seed(1)
splsFit <- train(x, logBBB, "spls",
                 tuneGrid = expand.grid(.K = 1:10, 
                   .eta = seq(0.1, 0.9, length = 20),
                   .kappa = 0.5),
                 trControl = trainControl(method = "LGOCV", p =.8, number = 100))

set.seed(1)
splsFit2 <- train(x, logBBB, "spls",
                 tuneGrid = expand.grid(.K = 4, 
                   .eta = .7,
                   .kappa = 0.5),
                 trControl = trainControl(method = "LGOCV", p =.8, number = 100))
splsNames <- colnames(splsFit2$finalModel$x)[splsFit2$finalModel$A]
splsCorr <- cor(x[, colnames(x) %in% splsNames])
summary(splsCorr[upper.tri(splsCorr)])

######################################################################
######################################################################


set.seed(1)
earthFit <- train(x, logBBB, "earth", tuneLength = 20,
                 trControl = trainControl(method = "LGOCV", p =.8, number = 100))
earthNames <- predictors(earthFit)
earthCorr <- cor(x[, colnames(x) %in% earthNames])
summary(earthCorr[upper.tri(earthCorr)])

######################################################################
######################################################################

set.seed(1)
gbmFit <- train(x, logBBB, "gbm",
                tuneGrid = expand.grid(.interaction.depth = 1:4,
                  .n.trees = (1:10) * 2, .shrinkage = 0.1),
                verbose = FALSE,
                trControl = trainControl(method = "LGOCV", p =.8, number = 100))


gbmNames <- predictors(gbmFit)
gbmCorr <- cor(x[, colnames(x) %in% gbmNames])
summary(gbmCorr[upper.tri(gbmCorr)])

######################################################################
######################################################################

enetResamp <- enetFit2$resample
enetResamp$Model <- paste("ElasticNet (",
                          length(enetCoef),
                          ")", sep = "")

splsResamp <- splsFit2$resample
splsResamp$Model <- paste("Sparse PLS (",
                          length(splsNames),
                          ")", sep = "")

earthResamp <- earthFit$resample
earthResamp$Model <- paste("Earth (",
                           length(earthNames),
                           ")", sep = "")

gbmResamp <- gbmFit$resample
gbmResamp$Model <- paste("Bootsted Tree (",
                         length(gbmNames),
                         ")", sep = "")


modelResamples <- rbind(enetResamp,
                        splsResamp,
                        earthResamp,
                        gbmResamp)
modelResamples$parameterGroup <- NULL

rmse <- modelResamples[, c("RMSE", "Model")]
names(rmse) <- c("Value", "Model")
rmse$Statistic <- "RMSE"


r2 <- modelResamples[, c("Rsquared", "Model")]
names(r2) <- c("Value", "Model")
r2$Statistic <- "Rsquared"

plotData <- rbind(rmse, r2)

trellis.par.set(caretTheme())
print(
      bwplot(Model ~ Value|Statistic,
       data = plotData,
       scales = list(x = list(relation = "free")),
       between = list(x = 1),
       xlab = "Hold-Out Performance"))

densityplot(~ RMSE, data = modelResamples,
            groups = Model,
            adjust = 1.25,
            auto.key = list(columns = 2),
            plot.points = FALSE )

densityplot(~ Rsquared, data = modelResamples,
            groups = Model,
            adjust = 1.25,
            auto.key = list(columns = 2),
            plot.points = FALSE )

bwplot(RMSE ~ Model, data = resamples, ylab = "Hold-Out RMSE")
bwplot(Rsquared ~ Model, data = resamples,
       ylab = paste("Hold-Out", expression(R^2)))


######################################################################
######################################################################


lmResample <- subset(as.data.frame(lmProfile$resample),
                     Variables == lmProfile$optsize)
lmResample$Variables <- NULL
lmResample$Model <- paste("RFE OLS (", length(lmProfile$optVariables),
                          ")", sep = "")

lmResample2 <- subset(as.data.frame(lmProfile2$resample),
                     Variables == lmProfile2$optsize)
lmResample2$Variables <- NULL
lmResample2$Model <- paste("RFE OLS wioth Re-rank (",
                           length(lmProfile2$optVariables),
                          ")", sep = "")



rfResample <- subset(as.data.frame(rfProfile$resample),
                     Variables == rfProfile$optsize)
rfResample$Variables <- NULL
rfResample$Model <- paste("RFE RandomForest (", length(rfProfile$optVariables),
                          ")", sep = "")

rfResample2 <- subset(as.data.frame(rfProfile2$resample),
                     Variables == rfProfile2$optsize)
rfResample2$Variables <- NULL
rfResample2$Model <- paste("RFE RandomForest Re-ranked (",
                           length(rfProfile2$optVariables),
                          ")", sep = "")

rfeResamples <- rbind(lmResample, lmResample2,
                      rfResample, rfResample2)

bwplot(Model ~ RMSE, data = resamples, xlab = "Hold-Out RMSE")
bwplot(Model ~ Rsquared, data = resamples,
       xlab = paste("Hold-Out", expression(R^2)))

