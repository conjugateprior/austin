\name{lbg}
\alias{lbg}
\docType{data}
\description{Example data from Laver Benoit and Garry (2003)}
\title{Example Data}
\usage{data(lbg)}
\details{
  This is the example word count data from Laver,
  Benoit and Garry's (2000) article
  on Wordscores.  Documents R1 to R5 are assumed to have known
  positions: -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed
  unknown.  The `correct' position for V1 is presumed to be -0.45.
  \code{\link{classic.wordscores}} generates approximately -0.45.

  To replicate the analysis in the paper, use the wordscores function
  either with identification fixing the first 5 document positions and
  leaving position of V1 to be predicted.
}

\references{
  Laver, Benoit and Garry (2003) `Estimating policy positions from
  political text using words as data' American Political Science Review
  97(2).
}

\keyword{datasets}
