\name{wordfreqj}
\alias{wordfreqj}
\title{Count Words in Documents}
\description{Create a word document count matrix from texts}
\usage{
wordfreqj(filenames, includenums=FALSE,
          renamecols=FALSE, stem=FALSE, 
          encoding=ifelse(.Platform$OS.type=="unix","UTF8","windows-1252"),
          verbose=FALSE)
}
\arguments{
  \item{filenames}{a list of filenames}
  \item{includenums}{if `TRUE' count occurrences of number words}
  \item{renamecols}{if `TRUE' rename columns}
  \item{stem}{if `TRUE' strip down to english stems}
  \item{encoding}{name of document encoding}
  \item{verbose}{show the command line used to run the java archive}
}
\value{
  An object of class wfm.
}
\details{
  Runs the java code (with no error checking!)
}
\author{Will Lowe}

