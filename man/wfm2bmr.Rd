\name{wfm2bmr}
\alias{wfm2bmr}
\title{Transform Word Frequency Matrix for BMR/BLR}
\description{Transforms a wfm to the format used by BMR/BLR}
\usage{wfm2bmr(y, wfm, filename)}
\arguments{
  \item{y}{integer dependent variable, may be NULL} 
  \item{wfm}{a word frequency matrix}
  \item{filename}{Name of the file to save data to}
}
\value{
  A file containing the variables in in sparse matrix format.
}
\details{
  BMR is sparse matrix format similar to that used by SVMlight

  Each line contains an optional dependent variable index and a sequence
  of indexes and feature value pairs divided by colons.  Indexes refer
  to the words with non-zero counts in the original matrix, and the
  feature values are the counts.
}
\seealso{
  \code{\link{wfm}}
}
\author{Will Lowe}
