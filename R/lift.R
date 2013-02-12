lift <- function(x, ...) UseMethod("lift")

lift.default <- function(x, ...) stop("'x' should be a formula")

lift.formula <- function(x, data = NULL, class = NULL,subset = TRUE,  lattice.options = NULL, labels = NULL, ...)
  {
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

    if(!is.null(labels))
      {
        if(length(labels) != length(probNames)) stop("labels should have an element for each term on the rhs of the formula")
        if(!all(probNames %in% names(labels))) stop(paste("labels should be a named vector or list with names:",
                                                          paste(probNames, collapse = ", ")))
      }

       
    
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
    plotData <- ddply(liftData, splitVars, liftCalc, class = class)
    if(!is.null(labels))
      {
        plotData$originalName <- plotData$liftModelVar
        plotData$liftModelVar <- as.character(plotData$liftModelVar)
        for(i in seq(along = labels)) plotData$liftModelVar[plotData$liftModelVar == names(labels)[i]] <- labels[i]
        plotData$liftModelVar <- factor(plotData$liftModelVar,
                                        levels = labels)
      }
    out <- list(data = plotData, class = class, probNames = probNames,
                pct =  mean(liftData$liftClassVar == class)*100, call = match.call())
    class(out) <- "lift"
    out
  }


print.lift <- function(x, ...)
  {
    printCall(x$call)
    cat("Models:", paste(unique(x$data$liftModelVar), collapse = ", "), "\n")
    cat("Event: ", x$class, " (", round( x$pct, 1), "%)\n", sep = "")      
    invisible(x)
  }


xyplot.lift <- function(x, data = NULL, plot = "gain", ...)
  {
    if(!(plot %in% c("lift", "gain"))) stop(paste("'plot' should be either 'lift' or 'gain'"))
    if(plot == "gain")
      {
        lFormula <- "CumEventPct ~ CumTestedPct"
        rng <- extendrange(c(0, 100))
        opts <- list(...)
        if(!any(names(opts) == "xlab")) opts$xlab <- "% Samples Tested"
        if(!any(names(opts) == "ylab")) opts$ylab <- "% Samples Found"
        if(!any(names(opts) == "type")) opts$type <- "l"
        if(!any(names(opts) == "ylim")) opts$ylim <- rng   
        if(!any(names(opts) == "xlim")) opts$xlim <- rng
        if(!any(names(opts) == "panel")) opts$panel <- panel.lift2
  } else {
        lFormula <- "lift ~ cuts"
        x$data <- x$data[order(x$data$liftModelVar, x$data$cuts),]
        rng <- extendrange(c(0, 100))
        opts <- list(...)
        if(!any(names(opts) == "xlab")) opts$xlab <- "Cut-Off"
        if(!any(names(opts) == "ylab")) opts$ylab <- "Lift"
        if(!any(names(opts) == "type")) opts$type <- "l"
  }
    args <- list(x = as.formula(lFormula),
                 data = x$data,
                 pct = x$pc)
    if(length(x$probNames) > 1) args$groups <- x$data$liftModelVar

    args <- c(args, opts)    
    do.call("xyplot", args)    
  }

liftCalc <- function(x, class = levels(x$liftClassVar)[1])
  {
    lvl <- levels(x$liftClassVar)
    x <- x[order(x$liftProbVar, decreasing = TRUE),]

    nEvents <- sum(x$liftClassVar == class)
    baseline <- mean(x$liftClassVar == class)
    cuts <- sort(unique(x$liftProbVar), decreasing = TRUE)
    cuts <- unique(c(1, cuts, 0))

    class2 <- levels(x$liftClassVar)
    class2 <- class2[class2 != class]
    tmp <- data.frame(cuts = cuts,
                      events = NA,
                      n = NA,
                      Sn = NA,
                      Sp = NA)
    for(i in seq(along = cuts))
      {
        sub <- x$liftClassVar[x$liftProbVar >= tmp$cuts[i]]
        tmp$n[i] <- length(sub)
        tmp$events[i] <- sum(sub == class)
        prd <- factor(ifelse(x$liftProbVar >= tmp$cuts[i], class, class2),
                      levels = levels(x$liftClassVar))
        tmp$Sn[i] <- sensitivity(prd,
                                 x$liftClassVar,
                                 positive = class)
        tmp$Sp[i] <- specificity(prd,
                                 x$liftClassVar,
                                 negative = class2)        
      }

    tmp$EventPct <- ifelse(tmp$n > 0, tmp$events/tmp$n*100, 0)
    tmp$CumEventPct <- tmp$events/nEvents*100
    tmp$lift <- tmp$events/tmp$n/baseline
    tmp$CumTestedPct <- tmp$n/nrow(x)*100
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


