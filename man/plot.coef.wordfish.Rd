\name{plot.coef.wordfish}
\alias{plot.coef.wordfish}
\title{Plot the Word Parameters From a Wordfish Model}
\description{Plots sorted beta and optionally also psi parameters from
  a  Wordfish model}
\usage{
\method{plot}{coef.wordfish}(x, pch=20, psi=TRUE, ...)
}

\arguments{
  \item{x}{a fitted Wordfish model}
  \item{pch}{Default is to use small dots to plot positions}
  \item{psi}{whether to plot word fixed effects}
  \item{...}{Any extra graphics parameters to pass in}
}
\value{
  A plot of sorted beta and optionally psi parameters.}
\seealso{
  \code{\link{wordfish}}
}
\author{Will Lowe}

