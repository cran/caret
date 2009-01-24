###################################################
### chunk number 1: loadLibs
###################################################
library(caret)
library(kernlab)
library(gbm)
library(ipred)
library(grid)
library(randomForest)
data(BloodBrain)
data(mdrr)


###################################################
### chunk number 2: preProc
###################################################
print(ncol(mdrrDescr))
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]

print(ncol(filteredDescr))

descrCor <- cor(filteredDescr)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]

print(ncol(filteredDescr))

set.seed(1)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

trainDescr <- filteredDescr[inTrain,]
testDescr <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

print(length(trainMDRR))
print(length(testMDRR))

preProcValues <- preProcess(trainDescr)

trainDescr <- predict(preProcValues, trainDescr)
testDescr <-  predict(preProcValues, testDescr)


###################################################
### chunk number 3: setup
###################################################
fitControl <- trainControl(
                           method = "LGOCV",
                           p = .75,
                           number = 30,
                           returnResamp = "all",
                           verboseIter = FALSE)
set.seed(2)


###################################################
### chunk number 4: mdrrModel1
###################################################
svmFit <- train(trainDescr, trainMDRR, method = "svmRadial", tuneLength = 4, trControl = fitControl)
svmFit


###################################################
### chunk number 5: setPageWidth
###################################################
currentWidth <- options("width")$width
options(width = 100)


###################################################
### chunk number 6: mdrrModel2
###################################################
gbmGrid <-  expand.grid(.interaction.depth = c(1, 3), .n.trees = c(100, 300, 500), .shrinkage = 0.1)
set.seed(3)
gbmFit <- train(trainDescr, trainMDRR, "gbm", tuneGrid = gbmGrid, trControl = fitControl, verbose = FALSE)
gbmFit


###################################################
### chunk number 7: resetPageWidth
###################################################
options(width = currentWidth)


###################################################
### chunk number 8: summaryFunc
###################################################
newSummary <- function (data, lev, model)
  {
    out <- c(sensitivity(data[, "pred"], data[, "obs"], lev[1]),
      specificity(data[, "pred"], data[, "obs"], lev[2]))
    
    names(out) <- c("Sens", "Spec")
    out
  }


###################################################
### chunk number 9: reTune
###################################################
fitControl$summaryFunction <- newSummary
set.seed(2)
svmNew <- train(trainDescr, trainMDRR, method = "svmRadial", metric = "Spec", tuneLength = 4, trControl = fitControl)
svmNew


###################################################
### chunk number 10: tolerance
###################################################
whichTwoPct <- tolerance(svmNew$results, "Spec", 2, TRUE)  
cat("best model within 2 pct of best:\n")
svmNew$results[whichTwoPct,]

whichSixPct <- tolerance(svmNew$results, "Spec", 6, TRUE)  
cat("\n\nbest model within 6 pct of best:\n")
svmNew$results[whichSixPct,]


###################################################
### chunk number 11: makePlots
###################################################
pdf(paste(getwd(), "/svm1.pdf", sep = ""), width = 5, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(svmFit))
dev.off()
pdf(paste(getwd(), "/svm2.pdf", sep = ""), width = 5, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(svmFit, xTrans = function(u) log(u, base = 10)))
dev.off()
pdf(paste(getwd(), "/gbm1.pdf", sep = ""), width = 5, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(gbmFit))
dev.off()
pdf(paste(getwd(), "/gbm2.pdf", sep = ""), width = 5, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(gbmFit, metric = "Kappa"))
dev.off()
pdf(paste(getwd(), "/gbm3.pdf", sep = ""), width = 5, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(gbmFit, meric = "Kappa", plotType = "level"))
dev.off()
pdf(paste(getwd(), "/resampHist.pdf", sep = ""), width = 7, height = 3.5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(resampleHist(svmFit, type = "density", layout = c(2, 1), adjust = 1.5))
dev.off()


###################################################
### chunk number 12: bhExample
###################################################
library(mlbench)
data(BostonHousing)
# we could use the formula interface too
bhDesignMatrix <-  model.matrix(medv ~. - 1, BostonHousing)


###################################################
### chunk number 13: bhSplit
###################################################
set.seed(4)
inTrain <- createDataPartition(BostonHousing$medv, p = .8, list = FALSE, times = 1)
trainBH <- bhDesignMatrix[inTrain,]
testBH <- bhDesignMatrix[-inTrain,]

preProc <- preProcess(trainBH)
trainBH <- predict(preProc, trainBH)
testBH  <- predict(preProc,  testBH)

trainMedv <- BostonHousing$medv[inTrain]
testMedv <- BostonHousing$medv[-inTrain]


###################################################
### chunk number 14: bhModels
###################################################
set.seed(5)
plsFit <- train(trainBH, trainMedv, "pls", tuneLength = 10, trControl = trainControl(verboseIter = FALSE))
set.seed(5)
marsFit <- train(trainBH, trainMedv, "earth", tuneLength = 10, trControl = trainControl(verboseIter = FALSE))



###################################################
### chunk number 15: plsPrediction1
###################################################
plsPred1 <- predict(plsFit$finalModel, newdata = as.matrix(testBH))
dim(plsPred1)


###################################################
### chunk number 16: plsPrediction1
###################################################
plsPred2 <- predict(plsFit, newdata = testBH)
length(plsPred2)


###################################################
### chunk number 17: bhPrediction1
###################################################
bhModels <- list(
                 pls = plsFit,
                 mars = marsFit)

bhPred1 <- predict(bhModels, newdata = testBH)
str(bhPred1)


###################################################
### chunk number 18: bhPrediction1
###################################################
allPred <- extractPrediction(bhModels,
                             testX = testBH,
                             testY = testMedv)
testPred <- subset(allPred, dataType == "Test")
head(testPred)

by(
   testPred, 
   list(model = testPred$model), 
   function(x) postResample(x$pred, x$obs))


###################################################
### chunk number 19: mbrConfusion
###################################################
mbrrPredictions <- extractPrediction(list(svmFit), testX = testDescr, testY = testMDRR)
mbrrPredictions <- mbrrPredictions[mbrrPredictions$dataType == "Test",]
sensitivity(mbrrPredictions$pred, mbrrPredictions$obs)
confusionMatrix(mbrrPredictions$pred, mbrrPredictions$obs)


###################################################
### chunk number 20: mbrrROC
###################################################
mbrrProbs <- extractProb(list(svmFit), testX = testDescr, testY = testMDRR)
mbrrProbs <- mbrrProbs[mbrrProbs$dataType == "Test",]
mbrrROC <- roc(mbrrProbs$Active, mbrrProbs$obs)
aucRoc(mbrrROC)


###################################################
### chunk number 21: mbrrPlots
###################################################
pdf(paste(getwd(), "/roc.pdf", sep = ""), width = 6.5, height = 7)
   plot(1 - mbrrROC[,"specificity"], mbrrROC[, "sensitivity"], type = "s", xlab = "1 - Specificity", ylab = "Sensitivity")
   abline(0,1, col = "grey", lty = 2)
dev.off()

pdf(paste(getwd(), "/svmProbs.pdf", sep = ""), width = 9, height = 7)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plotClassProbs(mbrrProbs))
dev.off()


###################################################
### chunk number 22: bhPredPlot
###################################################
trellis.par.set(caretTheme(), warn = FALSE)
print(plotObsVsPred(testPred))


