\name{initialize.urfish}
\alias{initialize.urfish}
\title{initialize.urfish}
\description{Get cheap starting values for a Wordfish model}
\usage{
initialize.urfish(tY)
}
\arguments{
  \item{tY}{a document by word matrix of counts}
}
\value{
  List with elements:
  \item{alpha}{starting values of alpha parameters}
  \item{psi}{starting values of psi parameters}
  \item{beta}{starting values of beta parameters}  
  \item{theta}{starting values for document positions}
}

\details{
  This function is only called by model fitting routines and
  does therefore not take a wfm classes.  tY is assumed to be in
  document by term form.

  In the poisson form of the model incidental parameters (alpha)
  are set to log(rowmeans/rowmeans[1]).
  intercept (psi) values are set to log(colmeans)
  These are subtracted from a the data matrix, which
  is logged and decomposed by SVD.  Word slope (beta) and
  document position (theta) are estimated by rescaling
  SVD output.
}
\references{
  This is substantially the method used by Slapin and Proksch's original code.
}
\author{Will Lowe}

