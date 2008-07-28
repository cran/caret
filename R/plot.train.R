"plot.train" <-
function(x, plotType = "scatter", metric = c("Accuracy", "RMSE"), digits = getOption("digits") - 5, xTrans = NULL, ...)
{
   if(!(plotType %in% c("level", "scatter", "line"))) stop("plotType must be either level, scatter or line")
   require(lattice)

   modelInfo <- modelLookup(x$method)

  
   if(all(modelInfo == "parameter")) 
      stop("This model type does not have a plot method (no tuning parameter)")
  
   performance <- x$results[, names(x$results) %in% metric]

   if(x$modelType == "Regression")
   {
      perfNames <- c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
   } else {
      perfNames <- c("Accuracy", "Kappa", "AccuracySD", "KappaSD")
   }   
   metricName <- metric[metric %in% perfNames]
   yLabel <- paste(x$control$method, "resampled training", tolower(metricName))   

   # check to see if some of the tuning parameters were not varied  
   tuneGrid <- x$results[, !(names(x$results) %in% perfNames), drop = FALSE]
   theseParam <- modelInfo$parameter
   tuneGrid <- tuneGrid[, theseParam,drop = FALSE]
 
   
   nonConstantParam <- apply(tuneGrid, 2, function(u) length(unique(u))) > 1
   numTune <- sum(nonConstantParam)
   if(numTune < 1) return(resampleHist(x, ...))
   tuneNames <- names(tuneGrid)[nonConstantParam]
    
    
   # we need to pretty-up some of the values 
   
   prettyVal <- function(u, dig) if(is.numeric(u)) signif(u, digits = dig) else as.character(u)

    
   # make a copy with only the tuning parameters which are changing
   resultsCopy <- x$results[, names(x$results) %in% c(metricName, tuneNames), drop = FALSE]
   resultsCopy <- resultsCopy[, c(metricName, tuneNames)]
   resultsCopy[,tuneNames] <- as.data.frame(
      lapply(
         resultsCopy[,tuneNames, drop = FALSE], 
         prettyVal, 
         dig = digits))
   
   if(x$method == "nb")
   {
      resultsCopy$usekernel <- factor(ifelse(resultsCopy$usekernel == 1, "Nonparametric", "Parametric"))  
   }
   
   interactionPlot <- function(x, y, groups, subscripts)
   {
      group.values <- unique(groups)
      for (i in seq(along=group.values)) 
      {
         id <- (groups[subscripts] == group.values[i])
         current.val <- group.values[i]
         panel.stripplot(
            x[id], y[id],
            jitter.data = FALSE, horizontal = FALSE,
            col = pchStyle$col[i], pch = pchStyle$pch[i])
         panel.linejoin(
            x[id], y[id], horizontal=F,
            col = lineStyle$col[i], lty = lineStyle$lty[i], lwd = lineStyle$lwd[i])
      }
   }

   pchStyle <- trellis.par.get("superpose.symbol")
   lineStyle <- trellis.par.get("superpose.line") 
  

   plotLabels <- modelInfo$label[theseParam %in% tuneNames]  
  
   if(numTune == 1)
   {
      
      if(plotType == "scatter")
      {
         if(is.null(xTrans))
         {
            xTrans = I
         } else {
            plotLabels <- paste(plotLabels, " (transformed)", sep = "")      
         }  
      }
      
      performancePlot <- switch(plotType,
         scatter = xyplot(
            resultsCopy[,metricName] ~ xTrans(resultsCopy[,2]), 
            xlab = plotLabels,
            ylab = yLabel, 
            type = "o",
            ...),
         level = bwplot(
            resultsCopy[,metricName] ~ factor(resultsCopy[,2]), 
            panel = "panel.xyplot", 
            type = "o", 
            xlab = plotLabels,
            ylab = yLabel, 
            ...),
         line = bwplot(
            resultsCopy[,metricName] ~ factor(resultsCopy[,2]), 
            panel = "panel.xyplot", 
            type = "o", 
            xlab = plotLabels,
            ylab = yLabel, 
            ...))   

   } else if(numTune == 2)
   {
      if(plotType == "scatter")
      {
         if(is.null(xTrans))
         {
            xTrans <- I
         } else {
            plotLabels[1] <- paste(plotLabels[1], " (transformed)", sep = "")      
         }  
      }
         
      performancePlot <- switch(plotType,      
      level = 
      {
         levelplot(
            resultsCopy[,metricName] ~ factor(resultsCopy[,2]) * factor(resultsCopy[,3]), 
            xlab = plotLabels[1], 
            ylab = plotLabels[2], 
            sub = yLabel, 
            ...)      
      },
      line = stripplot(
            resultsCopy[,metricName] ~ factor(resultsCopy[,2]),
            groups = factor(resultsCopy[,3]),
            panel = interactionPlot, 
            xlab = plotLabels[1], 
            ylab = yLabel, 
            key = list(
               title =plotLabels[2],  
               cex.title = 1,                  
               columns = min(3, length(unique(resultsCopy[,3]))),
               text=list(
                  lab = as.character(unique(signif(resultsCopy[,3], 3)))),
                  lines=list(
                     col=lineStyle$col[1:length(unique(resultsCopy[,3]))],
                     lwd = lineStyle$lwd[1:length(unique(resultsCopy[,3]))],
                     lty=lineStyle$lty[1:length(unique(resultsCopy[,3]))]),
                  points = list(
                     col=pchStyle$col[1:length(unique(resultsCopy[,3]))],
                     pch = pchStyle$pch[1:length(unique(resultsCopy[,3]))])),
            ...),
      scatter = 
      {
         xyplot(
               resultsCopy[,metricName] ~ xTrans(resultsCopy[,2]),
               groups = factor(prettyVal(resultsCopy[,3], digits)),
               type = "o", 
               xlab = plotLabels[1], 
               ylab = yLabel,  
               key = list(
                  title =plotLabels[2],  
                  cex.title = 1,                  
                  columns = min(3, length(unique(resultsCopy[,3]))),
                  text=list(
                     lab = as.character(unique(prettyVal(resultsCopy[,3], 3)))),
                     lines=list(
                        col=lineStyle$col[1:length(unique(resultsCopy[,3]))],
                        lwd = lineStyle$lwd[1:length(unique(resultsCopy[,3]))],
                        lty=lineStyle$lty[1:length(unique(resultsCopy[,3]))]),
                     points = list(
                        col=pchStyle$col[1:length(unique(resultsCopy[,3]))],
                        pch = pchStyle$pch[1:length(unique(resultsCopy[,3]))])),
               ...)               
      })
   
       
   } else if(numTune == 3)  
   {
   
      if(plotType == "scatter")
      {
         if(is.null(xTrans))
         {
            xTrans <- I
         } else {
            plotLabels[2] <- paste(plotLabels[2], " (transformed)", sep = "")      
         }  
      }
        
      stripVar <- paste(plotLabels[2], ": ", factor(paste(prettyVal(resultsCopy[,3], 3))), sep = "")
      
      performancePlot <- switch(plotType,      
         level = levelplot(
               resultsCopy[,metricName] ~ factor(resultsCopy[,2]) * factor(resultsCopy[,4])|stripVar, 
               xlab = plotLabels[1], 
               ylab = plotLabels[3], 
               sub =yLabel, ...),
         line = stripplot(
               resultsCopy[,metricName] ~ factor(resultsCopy[,2])|stripVar, 
               groups = factor(resultsCopy[,4]),
               panel = interactionPlot, 
               xlab = plotLabels[1], 
               ylab = yLabel,  
               key = list(
                  title =plotLabels[3],  
                  cex.title = 1,             
                  columns = min(4, length(unique(resultsCopy[,4]))),
                  text=list(
                     lab = as.character(unique(signif(resultsCopy[,4], 3)))),
                     lines=list(
                        col=lineStyle$col[1:length(unique(resultsCopy[,4]))],
                        lwd = lineStyle$lwd[1:length(unique(resultsCopy[,4]))],
                        lty=lineStyle$lty[1:length(unique(resultsCopy[,4]))]),
                     points = list(
                        col=pchStyle$col[1:length(unique(resultsCopy[,4]))],
                        pch = pchStyle$pch[1:length(unique(resultsCopy[,4]))])),
               ...),               
         scatter = xyplot(
               resultsCopy[,metricName] ~ xTrans(resultsCopy[,2])|stripVar, 
               groups = factor(resultsCopy[,4]),
               type = "o", 
               xlab = plotLabels[1], 
               ylab = yLabel, 
               key = list(
                  title =plotLabels[3],  
                  cex.title = 1,             
                  columns = min(4, length(unique(resultsCopy[,4]))),
                  text=list(
                     lab = as.character(unique(prettyVal(resultsCopy[,4], 3)))),
                     lines=list(
                        col=lineStyle$col[1:length(unique(resultsCopy[,4]))],
                        lwd = lineStyle$lwd[1:length(unique(resultsCopy[,4]))],
                        lty=lineStyle$lty[1:length(unique(resultsCopy[,4]))]),
                     points = list(
                        col=pchStyle$col[1:length(unique(resultsCopy[,4]))],
                        pch = pchStyle$pch[1:length(unique(resultsCopy[,4]))])),
               ...))
   }
   
   performancePlot
}


