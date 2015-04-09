#' Old-Style Wordscores
#' 
#' Construct a Wordscores model from reference document scores
#' 
#' This version of Wordscores is exactly as described in Laver et al.  2003 and
#' is provided for historical interest and continued replicability of older
#' analyses.
#' 
#' \code{scores} is a vector of document scores corresponding to the documents
#' in the word frequency matrix \code{wfm}.  The function computes wordscores
#' and returns a model from which virgin text scores can be predicted.
#' 
#' @param wfm object of class wfm
#' @param scores reference document positions/scores
#' @return An old-style Wordscores analysis.
#' @author Will Lowe
#' @export
#' @seealso \code{\link{summary.classic.wordscores}}
#' @references Laver, M. and Benoit, K. and Garry, J. (2003) 'Extracting policy
#' positions from political texts using words as data' American Political
#' Science Review. 97. pp.311-333
#' @examples
#' 
#' data(lbg)
#' ref <- getdocs(lbg, 1:5)
#' ws <- classic.wordscores(ref, scores=seq(-1.5,1.5,by=0.75))
#' summary(ws)
#' vir <- getdocs(lbg, 'V1') 
#' predict(ws, newdata=vir)
#' 
#' @export classic.wordscores
classic.wordscores <- function(wfm, scores){
    if (!is.wfm(wfm))
        stop("Function not applicable to this object")
    if (length(scores) != length(docs(wfm)))
        stop("There are not the same number of documents as scores")
    if (any(is.na(scores)))
        stop("One of the reference document scores is NA\nFit the model with known scores and use 'predict' to get virgin score estimates")

    thecall <- match.call()

    C.all <- as.worddoc(wfm)
    C <- C.all[rowSums(C.all)>0, ] ## just the words that occur
    F <- scale(C, center=FALSE, scale=colSums(C))
    ws <- apply(F, 1, function(x){ sum(scores * x) }) / rowSums(F)
    pi <- matrix(ws, nrow=length(ws))
    rownames(pi) <- rownames(C)
    colnames(pi) <- c("Score")

    val <- list(pi=pi,
                theta=scores,
                data=wfm,
                call=thecall)
    class(val) <- c('classic.wordscores', 'wordscores', class(val))

    return(val)
}

#' Summarize an Classic Wordscores Model
#' 
#' Summarises a Wordscores model
#' 
#' To see the wordscores, use \code{coef}.
#' 
#' @param object a fitted wordscores model
#' @param ... extra arguments (currently ignored)
#' @return A summary of information about the reference documents used to fit
#' the model.
#' @export
#' @author Will Lowe
#' @method summary classic.wordscores
summary.classic.wordscores <- function(object, ...){
    cat("Call:\n\t")
    print(object$call)

    cat("\nReference Document Statistics:\n\n")
    nn <- ifelse(wordmargin(object$data)==1,2,1)
    dd <- data.frame(Total=apply(object$data, nn, sum),
                     Min=apply(object$data, nn, min),
                     Max=apply(object$data, nn, max),
                     Mean=apply(object$data, nn, mean),
                     Median=apply(object$data, nn, median),
                     Score=object$theta)
    rownames(dd) <- docs(object$data)
    print(dd, digits=3)
    invisible(dd)
}

#' Show Wordscores
#' 
#' Lists wordscores from a fitted Wordscores model.
#' 
#' 
#' @param object a fitted Wordscores model
#' @param ... extra arguments, currently unused
#' @return The wordscores
#' @author Will Lowe
#' @seealso \code{\link{classic.wordscores}}
#' @method coef classic.wordscores
coef.classic.wordscores <- function(object, ...){
    return(object$pi)
}

#' Plot a Wordscores Model
#' 
#' Plots Wordscores from a fitted Wordscores model
#' 
#' 
#' @param x a fitted Wordscores model
#' @param ... other arguments, passed to the dotchart command
#' @return A plot of the wordscores in increasing order.
#' @author Will Lowe
#' @export
#' @seealso \code{\link{classic.wordscores}}
#' @method plot classic.wordscores
plot.classic.wordscores <- function(x, ...){
    ord <- order(x$pi)
    dotchart(x$pi[ord], labels=rownames(x$pi)[ord], ...)
}

#' Predict New Document Positions
#' 
#' Predicts positions of new documents from a fitted Wordscores model
#' 
#' This is the method described in Laver et al. 2003, including rescaling for
#' more than one virgin text.  Confidence intervals are not provided if
#' \code{rescale} is 'none'.
#' 
#' @param object Fitted wordscores model
#' @param newdata An object of class wfm in which to look for word counts to
#' predict document ideal points. If omitted, the reference documents are used.
#' @param rescale Rescale method for estimated positions.
#' @param z Notional confidence interval coverage
#' @param ... further arguments (quietly ignored)
#' @return \code{predict.wordscores} produces a vector of predicted document
#' positions and standard errors and confidence intervals.
#' @author Will Lowe
#' @export
#' @seealso \code{\link{classic.wordscores}}
#' @method predict classic.wordscores
predict.classic.wordscores <- function(object, newdata=NULL, rescale=c('lbg', 'none'), z=.95, ...){
    m <- object
    if (is.null(newdata))
        newd <- as.docword(m$data)
    else {
        if (is.wfm(newdata))
            newd <- as.docword(newdata)
        else
            stop("Use as.wfm to convert newdata into a suitable format")
    }

    scorable <- which(colnames(newd) %in% rownames(m$pi))
    pi <- as.vector(m$pi[ colnames(newd)[scorable], ])
    mess <- paste(length(scorable),
                  " of ",
                  length(colnames(newd)),
                  " words (",
                  round(100*length(scorable)/length(colnames(newd)), 2),
                  "%) are scorable\n\n",
                  sep="")
    cat(mess)
    scorable.newd <- subset(newd, select=scorable)

    preds <- apply(scorable.newd, 1, function(x){ sum(x*pi)/sum(x) }) ## point estimate
    rowsum <- rowSums(scorable.newd) ## doc lengths
    preds.se <- rep(0, length(preds))
    for (i in 1:length(preds)){
        preds.se[i] <- sqrt(sum(scorable.newd[i,] * (preds[i] - pi)**2 / rowsum[i])) /
            sqrt(rowsum[i])
    }
    rs <- match.arg(rescale)
    if (rs=='lbg'){
        SDr <- sd(m$theta)
        Sv <- mean(preds)
        SDv <- ifelse(length(preds)<2, 0, sd(preds))
        mult <- ifelse(SDv==0, 0, SDr/SDv)
        re.theta <- (preds - Sv) * mult + Sv
        if (mult==0){
            ## corner case for no variance pointing out the bogosity of rescaling
            int.high <- preds + z * preds.se
            int.low <- preds - z * preds.se
        } else {
            int.high <- ((preds + z * preds.se) - Sv) * mult + Sv
            int.low <- ((preds - z * preds.se) - Sv) * mult + Sv
        }
        dd <- matrix(cbind(preds, preds.se, re.theta, int.low, int.high), ncol=5)
        colnames(dd) <- c("Score", "Std. Err.", "Rescaled", "Lower", "Upper")
    } else {
        dd <- matrix(cbind(preds, se.pres=preds.se), ncol=2)
        colnames(dd) <- c("Score", "Std. Err.")
    }
    rownames(dd) <- rownames(scorable.newd)

    print(dd, digits=3)
    invisible(as.data.frame(dd))
}

