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

is.wfm <- function(x){
    nms <- names(dimnames(x))
    !is.null(nms) && identical(sort(nms), c('docs', 'words'))
}

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

as.docword <- function(wfm){
    if (wordmargin(wfm)==1)
        t(wfm)
    else
        wfm
}

as.worddoc <- function(wfm){
    if (wordmargin(wfm)==1)
        wfm
    else
        t(wfm)
}

words <- function(wfm){
    if (wordmargin(wfm)==1)
        rownames(wfm)
    else
	colnames(wfm)
}

'words<-' <- function(x, value){
    if (length(words(x)) != length(value))
        stop("Replacement values are not the same length as the originals")

    if (wordmargin(x)==1)
        rownames(x) <- value
    else
        colnames(x) <- value
    return(x)
}

docs <- function(wfm){
    if (wordmargin(wfm)==1)
        colnames(wfm)
    else
        rownames(wfm)
}

'docs<-' <- function(x, value){
    if (length(docs(x)) != length(value))
        stop("Replacement value is not the same length as original")

    if (wordmargin(x)==1)
        colnames(x) <- value
    else
        rownames(x) <- value
    return(x)
}

getdocs <- function(wfm, which){
    if (wordmargin(wfm)==1)
        wfm[, which, drop=FALSE]
    else {
        wfm[which, , drop=FALSE]
    }
}

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
