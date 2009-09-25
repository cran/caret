\name{plot.train}
\alias{plot.train}
\title{Plot Method for the train Class}
\description{
  This function takes the output of a \code{\link{train}} object and creates
      a line or level plot using the \code{lattice} library.
}
\usage{
\method{plot}{train}(x, 
     plotType = "scatter",
     metric = x$perfNames[1],
     digits = getOption("digits") - 5, 
     xTrans = NULL, 
     ...)
}
\arguments{
  \item{x}{an object of class \code{\link{train}}.}
  \item{metric}{What measure of performance to plot. Possible values are "RMSE", "Rsquared", "Accuracy" or "Kappa"}
  \item{plotType}{a string describing the type of plot (\code{"scatter"}, \code{"level"} or \code{"line"})}
  \item{digits}{an integer specifying the number of significant digits used to label the parameter value.}
  \item{xTrans}{a fuction that will be used to scale the x-axis in scatter plots.}
  \item{\dots}{specifications to be passed to \code{\link[lattice]{levelplot}} or \code{\link[lattice:xyplot]{bwplot}} (for line plots). 
   These values should not be axis labels, panel functions, or titles. }
}
\details{
If there are no tuning parameters, or none were varied, a plot of the resampling distirbution is produced via \code{\link{resampleHist}}.
  
If the model has one tuning parameter with multiple candidate values, a
plot is produced showing the profile of the results over the
parameter. Also, a plot can be produced if there are multiple
tuning parameters but only one is varied.

If there are two tuning parameters with different values, a 
plot can be produced where a different line is shown for each value of
of the other parameter. For three parameters, the same line plot is
created within conditioning panels of the other parameter.

Also, with two tuning parameters (with different values), a levelplot
(i.e. un-clustered heatmap) can be created. For more than two
parameters, this plot is created inside conditioning panels.

  
}
\author{Max Kuhn}

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{ \code{\link{train}} }
\examples{
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

library(e1071)
rpartFit <- train(TrainData, TrainClasses, "rpart", 
                  tuneLength=15)
plot(rpartFit, scales = list(x = list(rot = 90)))

library(klaR)
rdaFit <- train(TrainData, TrainClasses, "rda", 
                control = trainControl(method = "cv"))
plot(rdaFit, plotType = "line", auto.key = TRUE)
 }
   
\keyword{hplot} 
