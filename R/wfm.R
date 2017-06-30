#' Word Frequency Matrix
#' 
#' A word count matrix that know which margin holds the words.
#' 
#' If \code{mat} is a filename it should name a comma separated value format
#' with row labels in the first column and column labels in the first row.
#' Which represents words and which documents is specified by
#' \code{word.margin}, which defaults to words as rows.
#' 
#' A word frequency matrix is defined as any two dimensional matrix with
#' non-empty row and column names and dimnames 'words' and 'docs' (in either
#' order).  The actual class of such an object is not important for the
#' operation of the functions in this package, so wfm is essentially an
#' interface.  The function \code{\link{is.wfm}} is a (currently rather loose)
#' check whether an object fulfils the interface contract.
#' 
#' For such objects the convenience accessor functions \code{\link{as.docword}}
#' and \code{\link{as.worddoc}} can be used to to get counts whichever way up
#' you need them.
#' 
#' \code{\link{words}} returns the words and \code{\link{docs}} returns the
#' document titles.  \code{\link{wordmargin}} reminds you which margin contains
#' the words.  Assigning \code{wordmargin} flips the dimension names.
#' 
#' To get extract particular documents by name or index, use \link{getdocs}.
#' 
#' \code{\link{as.wfm}} attempts to convert things to be word frequency
#' matrices.  This functionality is currently limited to objects on which
#' \code{as.matrix} already works, and to \code{TermDocument} and
#' \code{DocumentTerm} objects from the \code{tm} package.
#' 
#' @param mat matrix of word counts or the name of a csv file of word counts
#' @param word.margin which margin holds the words
#' @return A word frequency matrix from a suitable object, or read from a file
#' if \code{mat} is character.  Which margin is treated as representing words
#' is set by \code{word.margin}.
#' @author Will Lowe
#' @seealso \code{\link{as.wfm}}, \code{\link{as.docword}},
#' \code{\link{as.worddoc}}, \code{\link{docs}}, \code{\link{words}},
#' \code{\link{is.wfm}}, \code{\link{wordmargin}}
#' @examples
#' 
#' mat <- matrix(1:6, ncol=2)	
#' rownames(mat) <- c('W1','W2','W3')
#' colnames(mat) <- c('D1','D2')
#' m <- wfm(mat, word.margin=1)
#' getdocs(as.docword(m), 'D2')
#' 
#' @importFrom methods is
#' @importFrom utils read.csv
#' @export
wfm <- function(mat, word.margin=1){
    if (is(mat, 'character')){
        if (!file.exists(mat))
            stop(paste("The file", mat, "does not exist"))
        mat <- read.csv(file=mat, row.names=1)
        #if (word.margin==2)
        #    mat <- t(mat)
    }

    if (is.null(rownames(mat)) || is.null(colnames(mat)))
        stop("Cannot convert this object to a wfm: It must have row and column names")

    if (!is(mat, 'matrix'))
        mat <- as.matrix(mat)

    if (word.margin==1)
        dimnames(mat) <- list(words=rownames(mat), docs=colnames(mat))
    else if (word.margin==2)
        dimnames(mat) <- list(docs=rownames(mat), words=colnames(mat))
    else
        stop("word.margin must be 1 (rows) or 2 (columns)")
    mat
}

#' Checks for Word Frequency Matrix
#' 
#' Checks whether an object is a Word Frequency Matrix
#' 
#' 
#' @param x a matrix of counts
#' @return Whether the object can be used as a Word Frequency Matrix
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export
is.wfm <- function(x){
    nms <- names(dimnames(x))
    !is.null(nms) && identical(sort(nms), c('docs', 'words'))
}

#' Which margin holds the words
#' 
#' Checks which margin (rows or columns) of a Word Frequency Matrix holds the
#' words
#' 
#' Changing the wordmargin by assignment just swaps the dimnames
#' 
#' @param x a word frequency matrix
#' @return 1 if words are rows and 2 if words are columns.
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export 
wordmargin <- function(x){
    ifelse(names(dimnames(x))[1]=='words', 1, 2)
}

'wordmargin<-' <- function(x, value){
    if (wordmargin(x) != value){
        ## relabel the dimensions but don't change their contents
        if (value==1)
            dimnames(x) <- list(words=rownames(x), docs=colnames(x))
        else
            dimnames(x) <- list(docs=rownames(x), words=colnames(x))
    }
    x
}

#' Coerce to a Word Frequency Matrix
#' 
#' Constructs a wfm object from various other kinds of objects
#' 
#' 
#' @param mat a matrix of counts
#' @param word.margin which margin of mat represents the words
#' @return an object of class wfm
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @importFrom methods is
#' @export
as.wfm <- function(mat, word.margin=1){
    ## rather speculative conversion from tm format here
    if (is(mat, 'TermDocumentMatrix'))
        return(wfm(as.matrix(mat), word.margin=1))

    if (is(mat, 'DocumentTermMatrix'))
        return(wfm(as.matrix(mat), word.margin=2))

    ## if it's a data.frame or a matrix this should work
    ## provided there are row and column names
    if (is.null(rownames(mat)) || is.null(colnames(mat)))
        stop("Cannot convert this object to a wfm.  It must have row and column names")

    wfm(mat, word.margin=word.margin)
}

#' Extract a Document by Word Matrix
#' 
#' Extract a word count matrix with documents as rows and words as columns
#' 
#' This is a helper function for wfm objects.  Use it instead of manipulating
#' wfm object themselves.
#' 
#' @param wfm an object of class wfm
#' @return a document by word count matrix
#' @author Will Lowe
#' @seealso \code{\link{as.worddoc}}, \code{\link{wfm}}
#' @export 
as.docword <- function(wfm){
    if (wordmargin(wfm)==1)
        t(wfm)
    else
        wfm
}

#' Extract a Word by Document Matrix
#' 
#' Extract a matrix of word counts with words as rows and documents as columns
#' 
#' This is a helper function for wfm objects.  Use it instead of manipulating
#' wfm object themselves.
#' 
#' @param wfm an object of class wfm
#' @return a word by document count matrix
#' @author Will Lowe
#' @seealso \code{\link{as.docword}}, \code{\link{wfm}}
#' @export 
as.worddoc <- function(wfm){
    if (wordmargin(wfm)==1)
        wfm
    else
        t(wfm)
}

#' Extract Words
#' 
#' Extracts the words from a wfm object
#' 
#' 
#' @param wfm an object of type wfm
#' @param value replacement if assignment
#' @return A list of words.
#' @author Will Lowe
#' @seealso \code{\link{wfm}}, \code{\link{docs}}
#' @export
words <- function(wfm){
    if (wordmargin(wfm)==1)
        rownames(wfm)
    else
	colnames(wfm)
}

#' @export 
#' @rdname words
'words<-' <- function(wfm, value){
    if (length(words(wfm)) != length(value))
        stop("Replacement values are not the same length as the originals")

    if (wordmargin(wfm)==1)
        rownames(wfm) <- value
    else
        colnames(wfm) <- value
    return(wfm)
}

#' Extract Document Names
#' 
#' Extracts the document names from a wfm object.
#' 
#' @param wfm an object of type wfm
#' @param value replacement if assignment
#' @return A list of document names.
#' 
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export
docs <- function(wfm){
    if (wordmargin(wfm)==1)
        colnames(wfm)
    else
        rownames(wfm)
}

#' @export 
#' @rdname docs
'docs<-' <- function(wfm, value){
    if (length(docs(wfm)) != length(value))
        stop("Replacement value is not the same length as original")

    if (wordmargin(wfm)==1)
        colnames(wfm) <- value
    else
        rownames(wfm) <- value
    return(wfm)
}

#' Get Documents
#' 
#' Gets particular documents from a wfm by name or index
#' 
#' getdocs is essentially a subset command that picks the correct margin for
#' you.
#' 
#' @param wfm a wfm object
#' @param which names or indexes of documents
#' @return A smaller wfm object containing only the desired documents with the
#' same word margin setting as the original matrix.
#' @author Will Lowe
#' @seealso \code{\link{as.wfm}}, \code{\link{as.docword}},
#' \code{\link{as.worddoc}}, \code{\link{docs}}, \code{\link{words}},
#' \code{\link{is.wfm}}, \code{\link{wordmargin}}
#' @export
getdocs <- function(wfm, which){
    if (wordmargin(wfm)==1)
        wfm[, which, drop=FALSE]
    else {
        wfm[which, , drop=FALSE]
    }
}

#' Trim a Word Frequency Data
#' 
#' Ejects low frequency observations and subsamples
#' 
#' 
#' @param wfm an object of class wfm, or a data matrix
#' @param min.count the smallest permissible word count
#' @param min.doc the fewest permissible documents a word can appear in
#' @param sample how many words to randomly retain
#' @param verbose whether to say what we did
#' @return If \code{sample} is a number then this many words will be retained
#' after \code{min.doc} and \code{min.doc} filters have been applied.
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export 
trim <- function(wfm, min.count=5, min.doc=5, sample=NULL, verbose=TRUE){
    ## eject words occurring less than min.count times and
    ## in fewer than min.doc documents
    ## and optionally sample a subset of words

    if (!is.wfm(wfm))
        stop("Function not applicable to this object")

    mY <- as.worddoc(wfm)

    rs1 <- which(rowSums(mY) >= min.count)
    if (verbose)
        cat("Words appearing less than", min.count, "times:", (nrow(mY) - length(rs1)), "\n")

    rs2 <- which(apply(mY, 1, function(x){ sum(x>0) >= min.doc } ))
    if (verbose)
        cat("Words appearing in fewer than", min.doc, "documents:", (nrow(mY) - length(rs2)), "\n")

    tokeep <- intersect(rs1, rs2)
    if (length(tokeep)==0)
        stop("No words left after trimming")

    if (!is.null(sample)){
        if (sample > length(tokeep))
            warning(paste('Sample size', sample, 'larger than',
                          length(tokeep), "already filtered from", nrow(mY), "so ignoring sampling request"))
        tokeep <- sample(tokeep, min(length(tokeep), sample))
        if (verbose)
            cat("Retaining a random sample of", sample, "words\n")
    }
    return(mY[sort(tokeep),])
}

#' Transform Word Frequency Matrix for lda
#' 
#' Transforms a wfm to the format used by the lda package
#' 
#' See the documentation of \code{lda} package for the relevant object
#' structures and file formats.
#' 
#' @param wfm a word frequency matrix
#' @param dir a file to dump the converted data
#' @param names Names of the data and vocabulary file respectively
#' @return A list containing \item{data}{zero indexed word frequency
#' information about a set of documents} \item{vocab}{a vocabulary list},
#' unless \code{dir} is specified.
#' 
#' If \code{dir} is specified then the same information is dumped to
#' 'vocab.dat' and 'mult.dat' in the \code{dir} folder.
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export
wfm2lda <- function(wfm, dir=NULL, names=c("mult.dat", "vocab.dat")){
    m <- as.worddoc(wfm)
    v <- words(m)

    d <- list()
    for (i in 1:length(docs(m))){
        nzero <- which(m[,i]>0)
        d[[i]] <- t(matrix(as.integer(c(nzero-1, m[nzero, i])), ncol=2))
    }
    if (!is.null(dir))
        return(list(vocab=v, data=d))

    if (!file.exists(dir))
        stop(paste("Folder", dir, "does not exist"))
    lines <- rep("", length(d))
    for (i in 1:length(d)){
        lines[i] <- paste(NCOL(d[[i]]),
                          paste(d[[i]][1,], ":", d[[i]][2,], sep="", collapse=" "))
    }

    writeLines(lines, file.path(dir, names[1]))
    writeLines(v, file.path(dir, names[2]))
}

#' Transform Word Frequency Matrix for BMR/BLR
#' 
#' Transforms a wfm to the format used by BMR/BLR
#' 
#' BMR is sparse matrix format similar to that used by SVMlight
#' 
#' Each line contains an optional dependent variable index and a sequence of
#' indexes and feature value pairs divided by colons.  Indexes refer to the
#' words with non-zero counts in the original matrix, and the feature values
#' are the counts.
#' 
#' @param y integer dependent variable, may be NULL
#' @param wfm a word frequency matrix
#' @param filename Name of the file to save data to
#' @return A file containing the variables in in sparse matrix format.
#' @author Will Lowe
#' @seealso \code{\link{wfm}}
#' @export
wfm2bmr <- function(y, wfm, filename){
    if (!is.null(y)){
        if (!(0 %in% y))
            stop("Dependent variable must index from 0")
        y <- as.numeric(y)+1 # to make 1 and 2 not 0 and 1
    }
    x <- as.worddoc(wfm)

    lines <- rep("", NCOL(x))
    for (i in 1:NCOL(x)){
        nonz <- which(x[,i] != 0.0)
        nonz.vals <- as.numeric(x[nonz,i])
        yy <- ifelse(!is.null(y), y[i], NULL)

        ldat <- paste(nonz, ":", nonz.vals, collapse=" ", sep="")
        lines[i] <- ifelse(is.null(y), ldat, paste(y[i], ldat))
    }

    writeLines(lines, filename)
}
