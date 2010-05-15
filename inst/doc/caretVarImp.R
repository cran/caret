###################################################
### chunk number 1: loadLibs
###################################################
library(caret)
library(randomForest)
data(mdrr)


###################################################
### chunk number 2: modelFits
###################################################
options(width = 80)
data(mdrr)

set.seed(100)

nzvColumns <- nearZeroVar(mdrrDescr)
mdrrDescr <- mdrrDescr[, -nzvColumns]

preProcVals <- apply(mdrrDescr, 2, processData)
mdrrDescr <- applyProcessing(mdrrDescr, preProcVals)


###################################################
### chunk number 3: <session
###################################################
toLatex(sessionInfo())


