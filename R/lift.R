

lift <- function(x, data = NULL, class = NULL, cuts = 11, subset = TRUE, lattice.options = NULL,
                 ylabel = "% Samples Found", xlabel = "% Samples Tested", ...)
  {
    library(lattice)
    library(plyr)

   if (!is.null(lattice.options)) {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }

    formula <- x
    groups  <- NULL
    subset <- eval(substitute(subset), data, environment(x))

    form <- latticeParseFormula(formula, data, subset = subset, 
                                groups = groups, multiple = TRUE, outer = TRUE, 
                                subscripts = TRUE, drop = TRUE)
    liftData <- data.frame(prob = form$y)
    probNames <- strsplit(form$right.name, " + ", fixed = TRUE)[[1]]

    liftData <- data.frame(liftClassVar = rep(form$left, length(probNames)),
                               liftProbVar = form$right)
    liftData$liftModelVar <- if(length(probNames) > 1) form$condition[[length(form$condition)]] else probNames
   
    if(length(form$condition) > 0 && any(names(form$condition) != ""))
      {
        ind <- sum(names(form$condition) != "")
        tmp <- as.data.frame(form$condition[1:ind])
        liftData <- cbind(liftData, tmp)
      }

    if(!is.factor(liftData$liftClassVar)) stop("the left-hand side of the formula must be a factor of classes")

    splitVars <- names(liftData)[!(names(liftData) %in% c("liftClassVar", "liftProbVar"))]

    if(is.null(class)) class <- levels(liftData$liftClassVar)[1]
    plotData <- ddply(liftData, splitVars, liftCalc, class = class, cuts = cuts)
    lFormula <- "cumPct ~ cuts"
    defaults <- c("liftModelVar", "cuts", "value", "resp", "n", "lift", "cumPct")
    extras <- names(plotData)[!(names(plotData) %in% defaults)]
    if(length(extras) > 0) lFormula <- paste(lFormula, paste(extras, collapse = "*"), sep = "|")


    rng <- extendrange(c(0, 100))
    if(length(probNames) > 1)
      {
        out <- xyplot(as.formula(lFormula), data = plotData,
                      groups = liftModelVar,
                      panel = panel.lift2,
                      pct = mean(liftData$liftClassVar == class)*100,
                      ylim = rng, xlim = rng,
                      xlab = xlabel, ylab = ylabel,
                      ...)
      } else {
        out <- xyplot(as.formula(lFormula), data = plotData,
                      panel = panel.lift2,
                      pct = mean(liftData$liftClassVar == class)*100,
                      ylim = rng, xlim = rng,
                      xlab = xlabel, ylab = ylabel,
                      ...)
      }
    out
    
  }

liftCalc <- function(x, class = levels(x$liftClassVar)[1], cuts = 11)
  {
    lvl <- levels(x$liftClassVar)
    x <- x[order(x$liftProbVar, decreasing = TRUE),]

    baseline <- mean(x$liftClassVar == class)
    cuts <- ceiling(seq(1, nrow(x), length = cuts))

    tmp <- data.frame(cuts = cuts/nrow(x),
                      value = cuts,
                      resp = NA,
                      n = NA)
    for(i in 2:length(cuts))
      {
        sub <- x[cuts[i-1]:(cuts[i] - 1),]
        tmp$n[i] <- nrow(sub)
        tmp$resp[i] <- sum(sub$liftClassVar == class)
      }

    tmp[1, c("value", "resp", "n")] <- 0
    tmp$lift <- tmp$resp/tmp$n/baseline
    tmp$cumPct <- cumsum(tmp$resp)/sum(x$liftClassVar == class)*100
    tmp$cuts <- tmp$cuts*100
    tmp
  }

panel.lift <- function(x,  y, ...)
{
  panel.xyplot(x, y, ...)
  panel.abline(0, 1, col = "black")  
}


panel.lift2 <- function (x, y, pct = 0, ...) 
{
  polyx <- c(0, pct, 100, 0)
  polyy <- c(0, 100, 100, 0)
  regionStyle <- trellis.par.get("reference.line")
  panel.polygon(polyx, polyy,
                col = regionStyle$col,
                border = regionStyle$col)
  panel.xyplot(x, y, ...)
}


