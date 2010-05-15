###################################################
### chunk number 1: startUp
###################################################
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)


###################################################
### chunk number 2: simSettings
###################################################
n <- 100
p <- 40
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
x <- cbind(sim$x,  matrix(rnorm(n * p), nrow = n))
y <- sim$y
colnames(x) <- paste("var", 1:ncol(x), sep = "")


###################################################
### chunk number 3: preProc
###################################################
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)
subsets <- c(1:5, 10, 15, 20, 25)


###################################################
### chunk number 4: lm
###################################################
set.seed(10)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "cv",
                   verbose = FALSE,
                   returnResamp = "final")

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile


###################################################
### chunk number 5: lmObjects
###################################################
predictors(lmProfile)
lmProfile$fit
lmProfile$resample


###################################################
### chunk number 6: lmPlot
###################################################
pdf("lm.pdf", width = 5, height = 7)
   trellis.par.set(caretTheme())
   plot1 <- plot(lmProfile, type = c("g", "o"))
   plot2 <- plot(lmProfile, type = c("g", "o"), metric = "Rsquared")
   print(update(plot1, 
                ylab = "Resampled RMSE"), 
         split=c(1,1,1,2), 
         more=TRUE)
   print(update(plot2, 
                ylab = "Resampled R^2"), 
         split=c(1,2,1,2))
dev.off()


###################################################
### chunk number 7: rfModel
###################################################
rfFuncs$fit


###################################################
### chunk number 8: rfPredict
###################################################
rfFuncs$pred


###################################################
### chunk number 9: rfRank
###################################################
rfFuncs$rank


###################################################
### chunk number 10: tolerance
###################################################
  example <- data.frame(RMSE = c(
                          3.215, 2.819, 2.414, 2.144, 
                          2.014, 1.997, 2.025, 1.987, 
                          1.971, 2.055, 1.935, 1.999, 
                          2.047, 2.002, 1.895, 2.018),
                        Variables = 1:16)
example


###################################################
### chunk number 11: tolerancePlot
###################################################

smallest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)

within10Pct <- pickSizeTolerance(example, metric = "RMSE", tol = 10, maximize = FALSE)

minRMSE <- min(example$RMSE)
example$Tolerance <- (example$RMSE - minRMSE)/minRMSE * 100   

pdf("tolerance.pdf", width = 4, height = 5)
par(mfrow = c(2, 1), mar = c(0, 4, 4, 2))

plot(example$Variables[-c(smallest, within10Pct)], 
     example$RMSE[-c(smallest, within10Pct)],
     ylim = extendrange(example$RMSE),
     ylab = "RMSE", xlab = "Variables")

points(example$Variables[smallest], 
       example$RMSE[smallest], pch = 16, cex= 1.3)

points(example$Variables[within10Pct], 
       example$RMSE[within10Pct], pch = 17, cex= 1.3)

 
  with(example, plot(Variables, Tolerance))
     abline(h = 10, lty = 2, col = "darkgrey")
dev.off()


###################################################
### chunk number 12: rfSelectVar
###################################################
rfFuncs$selectVar


###################################################
### chunk number 13: rf
###################################################
ctrl$functions <- rfFuncs
ctrl$returnResamp <- "all"
set.seed(10)
rfProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
print(rfProfile)


###################################################
### chunk number 14: rfPlot
###################################################
pdf("rf.pdf", width = 5, height = 7)
   trellis.par.set(caretTheme())
   plot1 <- plot(rfProfile, type = c("g", "o"))
   plot2 <- plot(rfProfile, type = c("g", "o"), metric = "Rsquared")
   print(update(plot1, 
                ylab = "Resampled RMSE"), 
         split=c(1,1,1,2), 
         more=TRUE)
   print(update(plot2, 
                ylab = "Resampled R^2"), 
         split=c(1,2,1,2))
dev.off()


###################################################
### chunk number 15: rfPlot
###################################################
pdf("rf2.pdf", width = 6, height = 7.5)
   trellis.par.set(caretTheme())
   plot1 <- xyplot(rfProfile, type = c("g", "p", "smooth"), ylab = "RMSE CV Estimates")
   plot2 <- densityplot(rfProfile, subset = Variables < 5, adjust = 1.25, as.table = TRUE, xlab = "RMSE CV Estimates")
   print(plot1, split=c(1,1,1,2), more=TRUE)
   print(plot2, split=c(1,2,1,2))
dev.off()


###################################################
### chunk number 16: rfSBFfit
###################################################
rfSBF$fit


###################################################
### chunk number 17: sbf
###################################################
set.seed(10)
rfWithFilter <- sbf(x, y,
                    sbfControl = sbfControl(
                      functions = rfSBF,
                      method = "cv",
                      verbose = FALSE))
print(rfWithFilter)


###################################################
### chunk number 18: session
###################################################
toLatex(sessionInfo())


