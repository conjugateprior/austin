\name{wfm2lda}
\alias{wfm2lda}
\title{Transform Word Frequency Matrix for lda}
\description{Transforms a wfm to the format used by the lda package}
\usage{wfm2lda(wfm, dir=NULL, names=c("mult.dat", "vocab.dat"))}
\arguments{
  \item{wfm}{a word frequency matrix}
  \item{dir}{a file to dump the converted data}
  \item{names}{Names of the data and vocabulary file respectively}
}
\value{
  A list containing
  \item{data}{zero indexed word frequency information about a set of documents}
  \item{vocab}{a vocabulary list}, unless \code{dir} is specified.

  If \code{dir} is specified then the
  same information is dumped to 'vocab.dat' and 'mult.dat' in the
  \code{dir} folder.
}
\details{
  See the documentation of \code{lda} package for the relevant object
  structures and file formats. 
}
\seealso{
  \code{\link{wfm}}
}
\author{Will Lowe}
