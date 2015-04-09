\name{sim.wordfish}
\alias{sim.wordfish}
\title{Simulate data and parameters for a Wordfish model}

\description{Simulates data and returns parameter values using Wordfish
  model assumptions: Counts are sampled under the assumption of
  independent Poisson draws with log expected means linearly related to
  a lattice of document positions.}

\usage{
sim.wordfish(docs=10, vocab=20, doclen=500,
             dist=c("spaced", "normal"),
             scaled=TRUE)
}
\arguments{
  \item{docs}{How many `documents' should be generated}
  \item{vocab}{How many `word' types should be generated}
  \item{doclen}{A scalar `document' length or vector of lengths}
  \item{dist}{the distribution of `document' positions}
  \item{scaled}{whether the document positions should be mean 0, unit sd}
}

\details{
  This function draws `docs' document positions from a
  Normal distribution, or regularly
  spaced between 1/`docs' and 1.

  `vocab'/2 word slopes are 1, the rest -1.
  All word intercepts are 0.  `doclen' words are then sampled
  from a multinomial with these parameters.

  Document position (theta) is sorted in increasing size across
  the documents.  If `scaled' is true it is normalized to mean zero,
  unit standard deviation.  This is most helpful when dist=normal.
}

\value{
  \item{Y}{A sample word-document matrix}
  \item{theta}{The `document' positions}
  \item{doclen}{The `document' lengths}
  \item{beta}{`Word' intercepts}
  \item{psi}{`Word' slopes}
}
\author{Will Lowe}
\seealso{}

