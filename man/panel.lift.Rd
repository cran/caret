\name{panel.lift2}
\alias{panel.lift}
\alias{panel.lift2}
\title{
Lattice Panel Functions for Lift Plots
}
\description{
Two panel functions that be used in conjunction with \code{\link{lift}}.
}
\usage{
panel.lift(x, y, ...)

panel.lift2(x, y, pct = 0, ...)
}
\arguments{
  \item{x}{
the percentage of searched to be plotted in the scatterplot
}
  \item{y}{
the percentage of events found to be plotted in the scatterplot
}
  \item{pct}{
the baseline percentage of true events in the data
}
  \item{\dots}{
options to pass to \code{\link[lattice:panel.xyplot]{panel.xyplot}}
}
}
\details{
\code{panel.lift} plots the data with a simple (black) 45 degree reference line.

\code{panel.lift2} is the default for \code{\link{lift}} and plots the
data points with a shaded region encompassing the space between to the random model and perfect model trajectories. The color of the region is determined by the lattice \code{reference.line} information (see example below).  
}


\author{
Max Kuhn
}

\seealso{
\code{\link{lift}}, \code{\link[lattice:panel.xyplot]{panel.xyplot}}, \code{\link[lattice:xyplot]{xyplot}}, \link[lattice:trellis.par.get]{trellis.par.set}}
\examples{
set.seed(1)
simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
                        perfect = sort(runif(200), decreasing = TRUE),
                        random = runif(200))

regionInfo <- trellis.par.get("reference.line")
regionInfo$col <- "lightblue"
trellis.par.set("reference.line", regionInfo)

lift2 <- lift(obs ~ random + perfect, data = simulated)
lift2
xyplot(lift2, auto.key = list(columns = 2))

## use a different panel function
xyplot(lift2, panel = panel.lift)
}

\keyword{hplot }

