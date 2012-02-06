lift <- function(x, ...) UseMethod("lift")

lift.default <- function(x, ...) stop("'x' should be a formula")

lift.formula <- function(x, data = NULL, class = NULL, cuts = NULL, subset = TRUE, lattice.options = NULL, ...)
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

    liftData <- data.frame(liftClassVar = rep(form$left, length(probNames)),
                           liftProbVar = form$right)
    liftData$liftModelVar <- if(length(probNames) > 1) form$condition[[length(form$condition)]] else probNames

    if(is.null(cuts))
      {
        cuts <- if(is.null(data)) nrow(liftData) else nrow(data)
      }
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
    out <- list(data = plotData, cuts = cuts, class = class, probNames = probNames,
                pct =  mean(liftData$liftClassVar == class)*100, call = match.call())
    class(out) <- "lift"
    out
  }


print.lift <- function(x, ...)
  {
    cat("\nCall:\n", truncateText(deparse(x$call, width.cutoff = 500)), "\n\n", sep = "")
    cat("Models:", paste(unique(x$data$liftModelVar), collapse = ", "), "\n")
    cat("Event: ", x$class, " (", round( x$pct, 1), "%)\n", sep = "")      
    cat("Cuts:", x$cuts, "\n")
    invisible(x)
  }


xyplot.lift <- function(x, data = NULL, ...)
  {
    lFormula <- "cumPct ~ cuts"
    defaults <- c("liftModelVar", "cuts", "value", "resp", "n", "lift", "cumPct")
    extras <- names(x$data)[!(names(x$data) %in% defaults)]
    if(length(extras) > 0) lFormula <- paste(lFormula, paste(extras, collapse = "*"), sep = "|")

    rng <- extendrange(c(0, 100))
    
    opts <- list(...)
    if(!any(names(opts) == "xlab")) opts$xlab <- "% Samples Tested"
    if(!any(names(opts) == "ylab")) opts$ylab <- "% Samples Found"
    if(!any(names(opts) == "type")) opts$type <- "S"
    if(!any(names(opts) == "ylim")) opts$ylim <- rng   
    if(!any(names(opts) == "xlim")) opts$xlim <- rng
    if(!any(names(opts) == "panel")) opts$panel <- panel.lift2
    
    args <- list(x = as.formula(lFormula),
                 data = x$data,
                 pct = x$pc)
    if(length(x$probNames) > 1) args$groups <- x$data$liftModelVar

    args <- c(args, opts)    
    do.call("xyplot", args)    
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


