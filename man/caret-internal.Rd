\name{caret-internal}
\title{Internal Functions}
\alias{createModel}
\alias{resampleWrapper}
\alias{sortImp}
\alias{caretTheme}
\alias{progress}

\description{Internal functions}
\usage{
createModel(data, method, tuneValue, obsLevels, pp = NULL, last = FALSE, custom = NULL, ...)
sortImp(object, top)
resampleWrapper(x, ind)
caretTheme()
rocPoint(cutoff, x, y, positive)
ipredStats(x)
rfStats(x)
bagEarthStats(x)
cforestStats(x)
progress(x, names, iter, start = TRUE)
}
\author{Max Kuhn, but \code{caretTheme} uses an expanded grid of the "Blues" palette designed by Cynthia Brewer and Mark Harrower}


\keyword{internal} 
