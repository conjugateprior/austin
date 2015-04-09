\name{plot.wordfish}
\alias{plot.wordfish}
\title{Plot a Wordfish Model}
\description{Plots a fitted Wordfish model with confidence intervals}
\usage{
\method{plot}{wordfish}(x, truevals=NULL, level=0.95, pch=20, ...)
}

\arguments{
  \item{x}{a fitted Wordfish model}
  \item{truevals}{True document positions if known}
  \item{level}{Intended coverage of confidence intervals}
  \item{pch}{Default is to use small dots to plot positions}
  \item{...}{Any extra graphics parameters to pass in}
}
\value{
  A plot of sorted estimated document positions, with confidence intervals and true
  document positions, if these are available.}
\seealso{
  \code{\link{wordfish}}
}
\author{Will Lowe}

