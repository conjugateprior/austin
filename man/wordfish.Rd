\name{wordfish}
\alias{wordfish}
\title{Estimate a Wordfish Model}
\description{
  Estimates a Wordfish model using Conditional Maximum Likelihood.
}
\usage{
wordfish(wfm, dir=c(1, 10), control=list(tol=1e-06,
         sigma=3, startparams=NULL), verbose=FALSE)
}
\arguments{
  \item{wfm}{a word frequency matrix}
  \item{dir}{set global identification by forcing \code{theta[dir[1]]} <
    \code{theta[dir[2]]}}
  \item{control}{list of estimation options}
  \item{verbose}{produce a running commentary}
}
\value{
  An object of class wordfish.  This is a list containing:

  \item{dir}{global identification of the dimension}  
  \item{theta}{document positions}
  \item{alpha}{document fixed effects}
  \item{beta}{word slope parameters}
  \item{psi}{word fixed effects}
  \item{docs}{names of the documents}
  \item{words}{names of words}
  \item{sigma}{regularization parameter for betas in poisson form}
  
  \item{ll}{final log likelihood}
  \item{se.theta}{standard errors for document position}
  \item{data}{the original data}
}
\details{
  Fits a Wordfish model with document ideal points constrained to mean
  zero and unit standard deviation.

  The \code{control} list specifies options for the estimation process.
  These are: \code{tol}, the proportional change in log likelihood
  sufficient to halt estimatioe, \code{sigma} the standard deviation
  for the beta prior in poisson form, and \code{startparams} a
  previously fitted wordfish model.  \code{verbose} generates
  a running commentary during estimation

  The model has two equivalent forms: a poisson model with two sets of
  document and two sets of word parameters, and a multinomial with
  two sets of word parameters and document ideal points.  The first form
  is used for estimation, the second for summarizing and prediction.
  
  The model is regularized by assuming a prior on beta with mean zero and
  standard deviation sigma (in poisson form).  If you don't want to
  regularize, set beta to a large number.
}
\references{
  Slapin and Proksch (2008) 'A Scaling Model for Estimating Time-Series
  Party Positions from Texts.' American Journal of Political Science 52(3):705-772.
}
\seealso{
  \code{\link{plot.wordfish}}, \code{\link{summary.wordfish}},
  \code{\link{coef.wordfish}}, \code{\link{fitted.wordfish}},
  \code{\link{predict.wordfish}}, \code{\link{sim.wordfish}}  
}
\examples{
dd <- sim.wordfish()
wf <- wordfish(dd$Y)
summary(wf)
}
\author{Will Lowe}
\keyword{}
