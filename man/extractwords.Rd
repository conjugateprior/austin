\name{extractwords}
\alias{extractwords}
\title{Pull Words From a List}
\description{Extract a list of matching words from another list of words}
\usage{
extractwords(words, patternfile, pattern.type=c('glob', 're'))
}
\arguments{
  \item{words}{the words against which patters are matched}
  \item{patternfile}{ file containing the patters to match, one per
    line}
  \item{pattern.type}{marks whether the patterns are 'globs' or full
    regular expressions}
}
\value{
  A list of matching words.
}
\author{Will Lowe}
