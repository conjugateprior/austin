\name{trim}
\alias{trim}
\title{Trim a Word Frequency Data}
\description{Ejects low frequency observations and subsamples}
\usage{
  trim(wfm, min.count=5, min.doc=5, sample=NULL)
}
\arguments{
  \item{wfm}{an object of class wfm, or a data matrix}
  \item{min.count}{the smallest permissible word count}
  \item{min.doc}{the fewest permissible documents a word can appear in}
  \item{sample}{how many words to randomly retain}
}
\value{
  If \code{sample} is a number then this many words will be retained
  after \code{min.doc} and \code{min.doc} filters have been applied.
}
\seealso{
  \code{\link{wfm}}
}
\author{Will Lowe}
