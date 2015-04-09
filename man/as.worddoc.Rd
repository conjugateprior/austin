\name{as.worddoc}
\alias{as.worddoc}
\title{Extract a Word by Document Matrix}
\description{Extract a matrix of word counts with words as rows and
  documents as columns
}
\usage{
as.worddoc(wfm)
}
\arguments{
  \item{wfm}{an object of class wfm}
}
\value{
  a word by document count matrix
}
\details{
  This is a helper function for wfm objects.  Use it instead of
  manipulating wfm object themselves.
}
\references{
}
\seealso{
  \code{\link{as.docword}}, \code{\link{wfm}}
}
\author{Will Lowe}
