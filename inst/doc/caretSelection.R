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
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y



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
### chunk number 7: rfeAlt
###################################################
rfRFE <-  list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   library(randomForest)
                   randomForest(x, y, importance = first, ...)
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
                 selectSize = pickSizeBest,
                 selectVar = pickVars)


###################################################
### chunk number 8: rfSummary
###################################################
rfRFE$summary


###################################################
### chunk number 9: rfModel
###################################################
rfRFE$fit


###################################################
### chunk number 10: rfPredict
###################################################
rfRFE$pred


###################################################
### chunk number 11: rfRank
###################################################
rfRFE$rank


###################################################
### chunk number 12: tolerance
###################################################
  example <- data.frame(RMSE = c(
                          3.215, 2.819, 2.414, 2.144, 
                          2.014, 1.997, 2.025, 1.987, 
                          1.971, 2.055, 1.935, 1.999, 
                          2.047, 2.002, 1.895, 2.018),
                        Variables = 1:16)
example


###################################################
### chunk number 13: tolerancePlot
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
### chunk number 14: rfSelectVar
###################################################
rfRFE$selectVar


###################################################
### chunk number 15: rf
###################################################
ctrl$functions <- rfRFE
ctrl$returnResamp <- "all"
set.seed(10)
rfProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
print(rfProfile)


###################################################
### chunk number 16: rfPlot
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
### chunk number 17: rfPlot
###################################################
pdf("rf2.pdf", width = 6, height = 7.5)
   trellis.par.set(caretTheme())
   plot1 <- xyplot(rfProfile, type = c("g", "p", "smooth"), ylab = "RMSE CV Estimates")
   plot2 <- densityplot(rfProfile, subset = Variables < 5, adjust = 1.25, as.table = TRUE, xlab = "RMSE CV Estimates")
   print(plot1, split=c(1,1,1,2), more=TRUE)
   print(plot2, split=c(1,2,1,2))
dev.off()


###################################################
### chunk number 18: rfSBFfit
###################################################
rfSBF$fit


###################################################
### chunk number 19: sbf
###################################################
set.seed(10)
rfWithFilter <- sbf(x, y,
                    sbfControl = sbfControl(
                      functions = rfSBF,
                      method = "cv",
                      verbose = FALSE))
print(rfWithFilter)


###################################################
### chunk number 20: resamps1
###################################################
bootValues <- resamples(
                        list(lmRFE = lmProfile, 
                             rfRFE = rfProfile, 
                             rfFilter = rfWithFilter))


###################################################
### chunk number 21: resamps2
###################################################
differences <- diff(bootValues)
summary(differences)


###################################################
### chunk number 22: resamps3
###################################################
pdf("resamps1.pdf", width = 5, height = 5)
   trellis.par.set(caretTheme())
   print(parallel(bootValues, metric = "Rsquared"))
dev.off()


###################################################
### chunk number 23: session
###################################################
toLatex(sessionInfo())


