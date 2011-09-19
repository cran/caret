\name{lift}
\alias{lift}
\title{
Lift Plot
}
\description{
For classification models, this function creates a 'lift plot' that describes how well a model ranks samples for one class
}
\usage{
lift(x, data = NULL, class = NULL, cuts = 11, 
     subset = TRUE, lattice.options = NULL, ...)
}
\arguments{
  \item{x}{
a \code{lattice} formula (see \code{\link[lattice:xyplot]{xyplot}} for syntax) where the left-hand side of the formula is a factor class variable of the observed outcome and the right-hand side specifies one or model columns corresponding to a numeric ranking variable for a model (e.g. class probabilities). The classification variable should have two levels. 
}
  \item{data}{
a data frame (or more precisely, anything that is a valid \code{envir} argument in \code{veal}, e.g., a list or an environment) containing values for any variables in the formula, as well as \code{groups} and \code{subset} if applicable. If not found in \code{data}, or if \code{data} is unspecified, the variables are looked for in the environment of the formula. 
}
  \item{class}{
a character string for the class of interest
}
  \item{cuts}{
the number of splits of the data are used to create the plot. 
}
  \item{subset}{
An expression that evaluates to a logical or integer indexing vector. It is evaluated in \code{data}. Only the resulting rows of \code{data} are used for the plot. 
}
  \item{lattice.options}{
A list that could be supplied to \code{\link[lattice:lattice.options]{lattice.options}} 
}
  \item{\dots}{
options to pass through to \code{\link[lattice:xyplot]{xyplot}} or the panel function
}
}
\details{
To construct the lift plot, the following steps are used for each model:

\enumerate{
   \item The data are ordered by the numeric model prediction used on the right-hand side of the model formula
   \item The data are split into \code{cuts - 1} roughly equal groups
   \item the number of samples with true results equal to \code{class} are determined
   \item the lift is calculated as the ratio of the percentage of samples in each split corresponding to \code{class} over the same percentage in the entire data set}
\code{lift} produces a plot of the cumulative lift values by the percentage of samples evaluated. 

This implementation uses the \pkg{lattice} function \code{\link[lattice:xyplot]{xyplot}}, so plot elements can be changed via panel functions, \link[lattice:trellis.par.get]{trellis.par.set} or other means.

}
\value{
a \pkg{lattice} object
}

\author{
Max Kuhn, some \pkg{lattice} code and documentation by Deepayan Sarkar
}

\seealso{
\code{\link[lattice:xyplot]{xyplot}}, \link[lattice:trellis.par.get]{trellis.par.set}} 

\examples{

panel.lift <- function(x,  y, ...)
{
  panel.xyplot(x, y, ...)
  panel.abline(0, 1, col = "black")  
}

set.seed(1)
simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
                        perfect = sort(runif(200), decreasing = TRUE),
                        random = runif(200))

lift(obs ~ random, data = simulated)
lift(obs ~ random, data = simulated, 
     panel = panel.lift, type = c("p", "l"))
lift(obs ~ random + perfect, data = simulated,
     panel = panel.lift, type = c("p", "l"),
     auto.key = list(columns = 2))


}
\keyword{hplot}