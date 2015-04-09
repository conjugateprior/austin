\name{summary.wordfish}
\alias{summary.wordfish}
\title{Summarize a Wordfish Model}
\description{Summarises estimated document positions from a fitted
  Wordfish model}
\usage{
\method{summary}{wordfish}(object, level=0.95, ...)
}
\arguments{
  \item{object}{fitted wordfish model}
  \item{level}{confidence interval coverage}
  \item{...}{extra arguments, e.g. level}
}
\value{
  A data.frame containing estimated document position with standard errors
  and confidence intervals.
}
\details{
  if `level' is passed to the function, e.g. 0.95 for 95 percent
  confidence, this generates the appropriate width intervals.
}

\seealso{\code{\link{wordfish}}}

\author{Will Lowe}

