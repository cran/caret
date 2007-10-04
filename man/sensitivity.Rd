\name{sensitivity}
\alias{sensitivity}
\alias{specificity}
\alias{negPredValue}
\alias{posPredValue}
\title{Calculate Sensitivity, Specificity and predictive values}
\description{
 These functions calculate the sensitivity, specificity or predictive values of a measurement system compared to a 
  reference results (the truth or a gold standard). The measurement and "truth" 
  data must have the same two possible outcomes and one of the outcomes 
  must be thought of as a "positive" results.
 
 The sensitivity is defined as the proportion of positive results out of the number of
 samples which were actually positive. When there are no positive results, sensitivity is
 not defined and a value of \code{NA} is returned. Similarly, when there are no negative 
 results, specificity is not defined and a value of \code{NA} is returned. Similar 
 statements are true for predictive values.
 
 The positive predictive value is defined as the percent of  predicted positives that 
 are actually positive while the negative predictive value is defined as the percent 
 of  negative positives that are actually negative.
}
\usage{
sensitivity(data, reference, positive = levels(reference)[1])
specificity(data, reference, negative = levels(reference)[2])
posPredValue(data, reference, positive = levels(reference)[1])
negPredValue(data, reference, negative = levels(reference)[2])
}

\arguments{
  \item{data}{a factor containing the discrete measurements}
  \item{reference}{a factor containing the reference values}
  \item{positive}{a character string that defines the factor level corresponding to 
  the "positive" results}
  \item{negative}{a character string that defines the factor level corresponding to 
  the "negative" results}  
}

\value{
   A number between 0 and 1 (or NA).
}

\author{Max Kuhn}

\examples{
data01 <- factor(c("A", "B", "B", "B"))
data02 <- factor(c("A", "B", "B", "B"))

ref01 <- factor(c("B", "B", "B", "B"))
ref02 <- factor(c("B", "A", "B", "B"))


table(data01, ref01)
sensitivity(data01, ref01)
posPredValue(data01, ref01)

table(data02, ref02)
sensitivity(data02, ref02)
posPredValue(data02, ref02)

data03 <- factor(c("A", "B", "B", "B"))
data04 <- factor(c("B", "B", "B", "B"))

ref03 <- factor(c("B", "B", "B", "B"), levels = c("A", "B"))
ref04 <- factor(c("B", "A", "B", "B"))


table(data03, ref03)
specificity(data03, ref03)
negPredValue(data03, ref03)

table(data04, ref04)
specificity(data04, ref04)
negPredValue(data04, ref04)


\dontshow{

   if(!isTRUE(all.equal(sensitivity(data01, ref01), .75))) stop("error in sensitivity test 1")

   if(!isTRUE(all.equal(sensitivity(data02, ref02), 0))) stop("error in sensitivity test 2")

   ref03 <- factor(c("B", "B", "B", "B"))

   if(!is.na(sensitivity(data02, ref03, "A"))) stop("error in sensitivity test3")
      
   options(show.error.messages = FALSE)
   test1 <-try(sensitivity(data02, as.character(ref03)))
   if(grep("Error", test1) != 1)
      stop("error in sensitivity calculation - allowed non-factors")
   options(show.error.messages = TRUE)
   
   
   ref03 <- factor(c("B", "B", "B", "B"), levels = c("A", "B"))
   
   if(!isTRUE(all.equal(specificity(data03, ref03), .75))) stop("error in specificity test 1")

   if(!isTRUE(all.equal(specificity(data04, ref04), 1.00))) stop("error in specificity test 2")

   if(!is.na(specificity(data01, ref01, "A"))) stop("error in specificity test3")
      
   options(show.error.messages = FALSE)
   test1 <-try(specificity(data04, as.character(ref03)))
   if(grep("Error", test1) != 1)
      stop("error in specificity calculation - allowed non-factors")
   options(show.error.messages = TRUE)
      
}


}
\keyword{manip}
