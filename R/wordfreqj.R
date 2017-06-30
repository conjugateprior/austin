#' Count Words in Documents
#' 
#' Create a word document count matrix from texts
#' 
#' Runs the java code (with no error checking!)
#' 
#' @param filenames a list of filenames
#' @param includenums if `TRUE' count occurrences of number words
#' @param renamecols if `TRUE' rename columns
#' @param stem if `TRUE' strip down to english stems
#' @param encoding name of document encoding
#' @param verbose show the command line used to run the java archive
#' @return An object of class wfm.
#' @author Will Lowe
#' @importFrom utils read.delim
#' @export wordfreqj
wordfreqj <- function(filenames,
                      includenums=FALSE,
                      renamecols=FALSE,
                      stem=FALSE,
                      encoding=ifelse(.Platform$OS.type=="unix","UTF8","windows-1252"),
                      verbose=FALSE){

    options.string <- ""
    if (includenums) {
        options.string <- paste(options.string, "-j")
    }
    if (renamecols) {
        options.string <- paste(options.string, "-k")
    }
    if (stem) {
        options.string <- paste(options.string, "-s")
    }
    options.string <- paste(options.string, "-e", encoding)

    fullpath <- system.file("java", "wordfreq.jar", package = "austin")
    if (verbose)
        cat("Looking at", fullpath, "for the java archive (jar)\n")

    cat("Running...")
    tmpfile <- tempfile('jfreq-')
    cmdline <- paste("java -Xmx1000m -jar",
                     shQuote(fullpath),
                     options.string,
                     paste(shQuote(filenames), collapse=" "),
                     "-o",
                     shQuote(tmpfile))
    if (verbose){ cat("\n", cmdline, "\n") }

    system(cmdline)
    cat("Done\n")

    fm <- read.delim(tmpfile, row.names=1)
    return( wfm(fm, word.margin=1) )
}

