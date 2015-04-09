\name{getdocs}
\alias{getdocs}
\title{Get Documents}
\description{Gets particular documents from a wfm by name or index}
\usage{getdocs(wfm, which)}
\arguments{
  \item{wfm}{a wfm object}
  \item{which}{names or indexes of documents}
}
\value{
  A smaller wfm object containing only the desired documents 
  with the same word margin setting as the original matrix.
}
\details{
  getdocs is essentially a subset command that picks the correct margin for you.
}
\seealso{\code{\link{as.wfm}},
  \code{\link{as.docword}},
  \code{\link{as.worddoc}},
  \code{\link{docs}},
  \code{\link{words}},
  \code{\link{is.wfm}},
  \code{\link{wordmargin}}
}
\author{Will Lowe}
