\name{coef.wordfish}
\alias{coef.wordfish}
\title{Extract Word Parameters}
\description{Extract word parameters beta and psi in an appropriate model parameterization}
\usage{
\method{coef}{wordfish}(object, form=c('multinomial', 'poisson'), ...)
}
\arguments{
  \item{object}{an object of class wordfish}
  \item{form}{which parameterization of the model to return parameters for}
  \item{...}{extra arguments}
}
\value{
  A data.frame of word parameters from a wordfish model
  in one or other parameterization.
}
\details{
  Slope parameters and intercepts are labelled beta and psi
  respectively.  In multinomial form the coefficient names reflect the
  fact that the first-listed word is taken as
  the reference category.  In poisson form, the coefficients are labeled
  by the words the correspond to.

  Note that in both forms there will be beta and psi
  parameters, so make sure they are the ones you want.
  
}
\seealso{
  \code{\link{wordfish}}
}
\author{Will Lowe}
