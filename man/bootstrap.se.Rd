\name{bootstrap.se}
\alias{bootstrap.se}
\title{Compute Bootstrap Standard Errors}
\description{
  Computes bootstrap standard errors for document positions from a
  fitted Wordfish model
}
\usage{bootstrap.se(object, L=50, verbose=FALSE, ...)}
\arguments{
  \item{object}{a fitted Wordfish model}
  \item{L}{how many replications}
  \item{verbose}{Give progress updates}
  \item{...}{Unused}
}
\value{
  Standard errors for document positions
}
\details{
  This function computes a parametric bootstrap by resampling
  counts from the fitted word counts, refitting the model, and
  storing the document positions.  The standard deviations for
  each resampled document position are returned.
}
\references{
}
\seealso{
}
\examples{
}
\author{Will Lowe}
\keyword{}
