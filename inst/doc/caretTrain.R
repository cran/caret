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

getInfo <- function(what = "Suggests")
{
  text <- packageDescription("caret")[what][[1]]
  text <- gsub("\n", ", ", text, fixed = TRUE)
  text <- gsub(">=", "$\\\\ge$", text, fixed = TRUE)
  eachPkg <- strsplit(text, ", ", fixed = TRUE)[[1]]
  
  out <- paste("\\\\texttt{", eachPkg[order(tolower(eachPkg))], "}", sep = "")
  paste(out, collapse = ", ")
}


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


###################################################
### chunk number 3: setup
###################################################
fitControl <- trainControl(method = "LGOCV",
                           p = .75,
                           number = 30,
                           returnResamp = "all",
                           verboseIter = FALSE)
set.seed(2)


###################################################
### chunk number 4: mdrrModel1
###################################################
svmFit <- train(trainDescr, trainMDRR, 
                method = "svmRadial", 
                preProcess = c("center", "scale"),
                tuneLength = 4, 
                trControl = fitControl)
svmFit


###################################################
### chunk number 5: getSeqMods
###################################################
seqModList <- paste(paste("\\\\texttt{", unique(subset(modelLookup(), seq)$model), "}", sep = ""), collapse = ", ")


###################################################
### chunk number 6: mdrrGrid
###################################################
gbmGrid <-  expand.grid(.interaction.depth = c(1, 3), 
                        .n.trees = c(10, 50, 100, 150, 200, 250, 300), 
                        .shrinkage = 0.1)


###################################################
### chunk number 7: mdrrGridFit
###################################################
set.seed(3)
gbmFit <- train(trainDescr, trainMDRR, 
                method = "gbm", 
                tuneGrid = gbmGrid, 
                trControl = fitControl, 
                ## This next option is directly passed 
                ## from train() to gbm()
                verbose = FALSE)
gbmFit


###################################################
### chunk number 8: summaryFunc
###################################################
Rand <- function (data, lev, model)
{
  library(e1071)
  tab <- table(data[, "pred"], data[, "obs"])
  out <- classAgreement(tab)$crand
  names(out) <- "cRand"
  out
}


###################################################
### chunk number 9: reTune
###################################################
fitControl$summaryFunction <- Rand
set.seed(2)
svmNew <- train(trainDescr, trainMDRR, 
                method = "svmRadial", 
                preProcess = c("center", "scale"),
                metric = "cRand", 
                tuneLength = 4, 
                trControl = fitControl)
svmNew


###################################################
### chunk number 10: svmRoc
###################################################
fitControl <- trainControl(method = "LGOCV",
                           p = .75,
                           number = 30,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           returnResamp = "all",
                           verboseIter = FALSE)
set.seed(2)
svmROC <- train(trainDescr, trainMDRR, 
                method = "svmRadial", 
                tuneLength = 4, 
                metric = "ROC",
                trControl = fitControl)
svmROC


###################################################
### chunk number 11: bestGBM
###################################################
printSelected <- function(x)
  {
    tmp <- x$bestTune
    names(tmp) <- gsub(".", " ", names(tmp), fixed = TRUE)
    tmp <- paste(names(tmp), "=", tmp)
    paste(tmp, collapse = ", ")
  }
getTrainPerf <- function(x)
  {
    bst <- x$bestTune
    names(bst) <- substring(names(bst), 2)
    merge(bst, x$results)
  }


###################################################
### chunk number 12: tolerance
###################################################
whichTwoPct <- tolerance(gbmFit$results, "Accuracy", 2, TRUE)  
cat("best model within 2 pct of best:\n")
gbmFit$results[whichTwoPct,]


###################################################
### chunk number 13: makePlots
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
pdf(paste(getwd(), "/gbm3.pdf", sep = ""), width = 5, height = 3.5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(plot(gbmFit, meric = "Kappa", plotType = "level"))
dev.off()



###################################################
### chunk number 14: bhExample
###################################################
library(mlbench)
data(BostonHousing)
# we could use the formula interface too
bhDesignMatrix <-  model.matrix(medv ~. - 1, BostonHousing)


###################################################
### chunk number 15: bhSplit
###################################################
set.seed(4)
inTrain <- createDataPartition(BostonHousing$medv, p = .8, list = FALSE, times = 1)
trainBH <- bhDesignMatrix[inTrain,]
testBH <- bhDesignMatrix[-inTrain,]

trainMedv <- BostonHousing$medv[inTrain]
testMedv <- BostonHousing$medv[-inTrain]


###################################################
### chunk number 16: bhModels
###################################################
set.seed(5)
plsFit <- train(trainBH, trainMedv, 
                "pls", 
                preProcess = c("center", "scale"),
                tuneLength = 10, 
                trControl = trainControl(verboseIter = FALSE,
                                         returnResamp = "all"))
set.seed(5)
marsFit <- train(trainBH, trainMedv, 
                 "earth", 
                 tuneLength = 10, 
                 trControl = trainControl(verboseIter = FALSE,
                                          returnResamp = "all"))



###################################################
### chunk number 17: marPrediction1
###################################################
marsPred1 <- predict(marsFit$finalModel, newdata = testBH)
head(marsPred1)


###################################################
### chunk number 18: marsPrediction2
###################################################
marsPred2 <- predict(marsFit, newdata = testBH)
head(marsPred2)


###################################################
### chunk number 19: bhPrediction1
###################################################
bhModels <- list(pls = plsFit,
                 mars = marsFit)

bhPred1 <- predict(bhModels, newdata = testBH)
str(bhPred1)


###################################################
### chunk number 20: bhPrediction1
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
### chunk number 21: mbrConfusion
###################################################
mbrrPredictions <- extractPrediction(list(svmFit), testX = testDescr, testY = testMDRR)
mbrrPredictions <- mbrrPredictions[mbrrPredictions$dataType == "Test",]
sensitivity(mbrrPredictions$pred, mbrrPredictions$obs)
confusionMatrix(mbrrPredictions$pred, mbrrPredictions$obs)


###################################################
### chunk number 22: mbrrROC
###################################################
mbrrProbs <- extractProb(list(svmFit), testX = testDescr, testY = testMDRR)
mbrrProbs <- mbrrProbs[mbrrProbs$dataType == "Test",]
mbrrROC <- roc(mbrrProbs$Active, mbrrProbs$obs)
aucRoc(mbrrROC)


###################################################
### chunk number 23: mbrrPlots
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
### chunk number 24: bhPredPlot
###################################################
pdf("bhPredPlot.pdf", width = 8, height = 5)
trellis.par.set(caretTheme(), warn = FALSE)
print(plotObsVsPred(testPred))
dev.off()


###################################################
### chunk number 25: makeResampPlots
###################################################
pdf(paste(getwd(), "/marsXY.pdf", sep = ""), width = 7, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(xyplot(marsFit, type= c("g", "p", "smooth"), degree = 2))
dev.off()
pdf(paste(getwd(), "/marsDens.pdf", sep = ""), width = 7, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(densityplot(marsFit, as.table = TRUE, subset = nprune < 10))
dev.off()


###################################################
### chunk number 26: loadData
###################################################
## If we compute the above models, the vignettes takes too long for cran, 
## so we load the data from a remote source
load(url("http://caret.r-forge.r-project.org/Classification_and_Regression_Training_files/exampleModels.RData"))


###################################################
### chunk number 27: resamps
###################################################
resamps <- resamples(list(CART = rpartFit,
                          CondInfTree = ctreeFit,
                          MARS = earthFit,
                          M5 = m5Fit))
resamps
summary(resamps)


###################################################
### chunk number 28: resamplePlots
###################################################
bwplot(resamps, metric = "RMSE")

densityplot(resamps, metric = "RMSE")

xyplot(resamps,
       models = c("CART", "MARS"),
       metric = "RMSE")

splom(resamps, metric = "RMSE")


###################################################
### chunk number 29: resamplePlots2
###################################################
pdf("resampleScatter.pdf", width = 6, height = 6)
trellis.par.set(caretTheme())
print(xyplot(resamps, models = c("CART", "MARS")))
dev.off()
pdf("resampleDens.pdf", width = 7, height = 4.5)
print(
      densityplot(resamps, 
                  scales = list(x = list(relation = "free")), 
                  adjust = 1.2, 
                  plot.points = FALSE, 
                  auto.key = list(columns = 2)))

dev.off() 


###################################################
### chunk number 30: diffs
###################################################
difValues <- diff(resamps)

difValues

summary(difValues)


###################################################
### chunk number 31: diffPlots
###################################################
dotplot(difValues)

densityplot(difValues,
            metric = "RMSE",
            auto.key = TRUE,
            pch = "|")

bwplot(difValues,
       metric = "RMSE")

levelplot(difValues, what = "differences")


###################################################
### chunk number 32: diffPlots2
###################################################
pdf("diffLevel.pdf", width = 7.5, height = 6)
trellis.par.set(caretTheme())
print(levelplot(difValues, what = "differences"))
dev.off()

pdf("diffDot.pdf", width = 6, height = 6)
plotTheme <- caretTheme()
plotTheme$plot.symbol$pch <- 16
plotTheme$plot.line$col <- "black"
trellis.par.set(plotTheme)
print(dotplot(difValues))
dev.off()



###################################################
### chunk number 33: session
###################################################
toLatex(sessionInfo())


