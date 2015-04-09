\name{predict.classic.wordscores}
\alias{predict.classic.wordscores}
\title{Predict New Document Positions}
\description{Predicts positions of new documents from a fitted Wordscores model}

\usage{
\method{predict}{classic.wordscores}(object, newdata=NULL, rescale=c('lbg', 'none'), z=.95, ...)
}
\arguments{
      \item{object}{Fitted wordscores model}
      \item{newdata}{An object of class wfm
	in which to look for word counts to predict document ideal
	points. If omitted, the reference documents are used.}
      \item{rescale}{Rescale method for estimated positions.}
      \item{z}{Notional confidence interval coverage} 
      \item{...}{further arguments (quietly ignored)}
}
\value{
   \code{predict.wordscores} produces a vector of predicted document positions and 
   standard errors and confidence intervals.
}
\details{
  This is the method described in Laver et al. 2003, including rescaling for
  more than one virgin text.  Confidence intervals are
  not provided if \code{rescale} is 'none'.
}
\seealso{\code{\link{classic.wordscores}}}

\author{Will Lowe}
