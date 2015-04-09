plot.positions <- function(pos, lower, upper, sort=TRUE, labels=FALSE,
                           pch=20, ylab="", xlab="Position", ...){
    if (sort){
        ord <- order(pos)
        pos <- pos[ord]
        upper <- upper[ord]
        lower <- lower[ord]
    }
    print(pos)
    print(lower)
    print(upper)
    plot(pos, 1:length(pos), xlab=xlab, ylab=ylab, pch=pch, ...)
    segments(lower, 1:length(pos), upper, 1:length(pos))
    if (!is.null(labels)){
        text(upper, 1:length(pos), labels=labels, pos=4)
    }
}

plot.positions.group <- function(x, x.label, groups, names, ...) {
    ## a box plots of positions by party
    ## for sorting by median score
    tMedians <- aggregate(x, list(groups), median, na.rm=T)
    party2 <- factor(groups, levels = tMedians[order(tMedians$x), 1])
    par(cex.axis=1)
    print(tMedians)
    dotchart(x, groups=groups, labels=names, ...)
}
