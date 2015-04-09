\name{wordmargin}
\alias{wordmargin}
\title{Which margin holds the words}
\description{Checks which margin (rows or columns) of a Word Frequency Matrix holds the words}
\usage{wordmargin(x)}
\arguments{
  \item{x}{a word frequency matrix}
}
\value{1 if words are rows and 2 if words are columns.}
\details{Changing the wordmargin by assignment just swaps the dimnames}
\seealso{\code{\link{wfm}}}
\author{Will Lowe}
