\name{as.docword}
\alias{as.docword}
\title{Extract a Document by Word Matrix}
\description{Extract a word count matrix with documents as rows and
  words as columns}
\usage{
as.docword(wfm)
}
\arguments{
  \item{wfm}{an object of class wfm}
}
\value{
  a document by word count matrix
}
\details{
  This is a helper function for wfm objects.  Use it instead of
  manipulating wfm object themselves.
}
\seealso{ \code{\link{as.worddoc}}, \code{\link{wfm}} }

\author{Will Lowe}
