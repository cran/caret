###################################################
### chunk number 1: loadLibs
###################################################
library(caret)
library(kernlab)
library(gbm)
library(ipred)
library(grid)
library(pls)
library(randomForest)
data(BloodBrain)
data(mdrr)


###################################################
### chunk number 2: oilData
###################################################
data(oil)
dim(fattyAcids)
table(oilType)


###################################################
### chunk number 3: nzvDescr
###################################################
data.frame(table(mdrrDescr$nR11))


###################################################
### chunk number 4: nzvEx
###################################################
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]


###################################################
### chunk number 5: nzvReduce
###################################################
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]


###################################################
### chunk number 6: tmpCorr
###################################################
descrCor <- abs(cor(filteredDescr))


###################################################
### chunk number 7: corr
###################################################
descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])


###################################################
### chunk number 8: linearCombData
###################################################
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)
ltfrDesign


###################################################
### chunk number 9: linearComb
###################################################
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]
findLinearCombos(ltfrDesign[, -comboInfo$remove])


###################################################
### chunk number 10: centerScale
###################################################
set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainDescr <- predict(preProcValues, training)
testDescr <- predict(preProcValues, test)


###################################################
### chunk number 11: splomBefore
###################################################
plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")]))
trellis.par.set(caretTheme(), warn = FALSE)
print(xyplot(nC ~ X4v, plotSubset, groups = mdrrClass, auto.key = list(columns = 2)))


###################################################
### chunk number 12: splomAfter
###################################################
trellis.par.set(caretTheme(), warn = FALSE)
print(xyplot(nC ~ X4v, spatialSign(plotSubset), groups = mdrrClass, auto.key = list(columns = 2)))


###################################################
### chunk number 13: mbrrSplit1
###################################################
set.seed(3456)
set1index <- createDataPartition(mdrrClass, p = .8, list = FALSE, times = 1)
set1 <- mdrrClass[set1index]
round(table(set1)/length(set1), 2)
round(table(mdrrClass)/length(mdrrClass), 2)


###################################################
### chunk number 14: diss
###################################################
pdf(paste(getwd(), "/diss.pdf", sep = ""), width = 7, height = 9.5)

data(cox2)

testing <- scale(cox2Descr[, c(102, 30)])
set.seed(1)
startSet <- sample(1:dim(testing)[1], 5)
samplePool <- testing[-startSet,]
start <- testing[startSet,]


par(mfrow=c(3,2))

plot(testing[,1], testing[,2], xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
title("Original Data")

subsetSize <- 20

########################################################

newSamp <- maxDissim(start, samplePool, n = subsetSize)

plot(samplePool[,1], samplePool[,2], type = "n", xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
points(samplePool[-newSamp,1], samplePool[-newSamp,2], col = "grey")

text(samplePool[newSamp,1], samplePool[newSamp,2], paste(1:subsetSize), col = "blue", cex = 1.2)
title("Euclidean Dist with MaxMin")

text(start[,1], start[,2], rep("S", dim(start)[1]), col = "red", cex = 1.2)


########################################################

newSamp <- maxDissim(start, samplePool, n = subsetSize, obj = sumDiss)

plot(samplePool[,1], samplePool[,2], type = "n", xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
points(samplePool[-newSamp,1], samplePool[-newSamp,2], col = "grey")

text(samplePool[newSamp,1], samplePool[newSamp,2], paste(1:subsetSize), col = "blue", cex = 1.2)
title("Euclidean Dist with MaxSum")

text(start[,1], start[,2], rep("S", dim(start)[1]), col = "red", cex = 1.2)

########################################################

newSamp <- maxDissim(start, samplePool, n = subsetSize, obj = sumDiss, randomFrac = 0.05)

plot(samplePool[,1], samplePool[,2], type = "n", xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
points(samplePool[-newSamp,1], samplePool[-newSamp,2], col = "grey")

text(samplePool[newSamp,1], samplePool[newSamp,2], paste(1:subsetSize), col = "blue", cex = 1.2)
title("Euclidean Dist with MaxSum and 5% Sampling")

text(start[,1], start[,2], rep("S", dim(start)[1]), col = "red", cex = 1.2)



########################################################


newSamp <- maxDissim(start, samplePool, n = subsetSize, method = "Manhattan")

plot(samplePool[,1], samplePool[,2], type = "n", xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
points(samplePool[-newSamp,1], samplePool[-newSamp,2], col = "grey")

text(samplePool[newSamp,1], samplePool[newSamp,2], paste(1:subsetSize), col = "blue", cex = 1.2)
title("Manhattan Dist with MaxMin")

text(start[,1], start[,2], rep("S", dim(start)[1]), col = "red", cex = 1.2)


########################################################


newSamp <- maxDissim(start, samplePool, n = subsetSize, method = "Manhattan", obj = sumDiss)

plot(samplePool[,1], samplePool[,2], type = "n", xlab = "Total positive partial charge", ylab = "Predicted Skin Permeability")
points(samplePool[-newSamp,1], samplePool[-newSamp,2], col = "grey")

text(samplePool[newSamp,1], samplePool[newSamp,2], paste(1:subsetSize), col = "blue", cex = 1.2)
title("Manhattan Dist with MaxSum")

text(start[,1], start[,2], rep("S", dim(start)[1]), col = "red", cex = 1.2)


dev.off()


###################################################
### chunk number 15: featurePlot
###################################################
exDescr <- c("Wap", "G.N..N.", "SEigp")
mdrrPreProc1 <- apply(mdrrDescr[,exDescr] , 2, processData)

mdrrProc <- applyProcessing(mdrrDescr[,exDescr], mdrrPreProc1)

pdf(paste(getwd(), "/fp1.pdf", sep = ""), width = 7, height = 7)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(featurePlot(mdrrProc, mdrrClass, "ellipse"))
dev.off()

pdf(paste(getwd(), "/fp2.pdf", sep = ""), width = 7, height = 3.5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(featurePlot(mdrrProc, mdrrClass, 
      "density", scales = list(x = list(relation="free"), 
      y = list(relation="free")), adjust = 1.5, pch = "|", layout = c(3,1), 
      auto.key = list(columns = 3)))
dev.off()

pdf(paste(getwd(), "/fp3.pdf", sep = ""), width = 7, height = 3.5)
   trellis.par.set(caretTheme(), warn = FALSE)
   print(featurePlot(mdrrProc, mdrrClass, "boxplot", 
      scales = list(y = list(relation="free")), layout = c(3,1)))
dev.off()

pdf(paste(getwd(), "/fp4.pdf", sep = ""), width = 7, height = 3.5)
   data(BloodBrain)
   trellis.par.set(caretTheme(), warn = FALSE)
   exDescr <- c("psa_npsa", "clogp", "vsa_acc")   
   print(featurePlot(bbbDescr[,exDescr], logBBB, layout = c(3,1)))
dev.off()


###################################################
### chunk number 16: plsProb
###################################################
bTime <- system.time(Bayes <- plsda(trainDescr, trainMDRR, ncomp = 5, probMethod = "Bayes"))[3]
sTime <- system.time(softmax <- plsda(trainDescr, trainMDRR, ncomp = 5))[3]

bProbs <- as.data.frame(predict(Bayes, as.matrix(testDescr), type = "prob"))
bProbs$method <- "Bayes Rule"
bProbs$obs <- testMDRR
sProbs <- as.data.frame(predict(softmax, as.matrix(testDescr), type = "prob"))
sProbs$method <- "Softmax"
sProbs$obs <- testMDRR
names(sProbs) <- names(bProbs)
both <- rbind(bProbs, sProbs)

bPerf <- confusionMatrix(predict(Bayes, as.matrix(testDescr)),
                         testMDRR)
sPerf <- confusionMatrix(predict(softmax, as.matrix(testDescr)),
                         testMDRR)
                                 
pdf(paste(getwd(), "/plsProbs.pdf", sep = ""), width = 7, height = 5)
   trellis.par.set(caretTheme(), warn = FALSE)
print(
      histogram(
                ~Active|method*obs, data = both, 
                xlab = "Active Probability",
                as.table = TRUE))
dev.off()


