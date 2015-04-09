\name{rescale}
\alias{rescale}
\title{Rescale Estimated Document Positions}
\description{
  Linearly rescales estimated document positions on the basis of two
  control points.
}
\usage{
rescale(object, ident=c(1,-1,10,1))
}
\arguments{
  \item{object}{fitted wordfish or wordscores object}
  \item{ident}{two documents indexes and and their desired new positions}
}
\value{
  A data frame containing the rescaled document positions with standard
  errors if available.
}
\details{
  The rescaled positions set document with index ident[1] to position
  ident[2] and docuemnt with index ident[3] to position ident[4].  The
  fitted model passed as the first argument is not affected.
}
\references{
}
\seealso{
}
\examples{
}
\author{Will Lowe}
\keyword{}
