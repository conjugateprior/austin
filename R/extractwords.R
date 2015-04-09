extractwords <- function(words,
                         patternfile,
                         pattern.type=c('glob', 're')){
    pats <- read.table(patternfile, strip.white=TRUE)
    if (pattern.type[1] == 'glob')
        pats <- apply(pats, 1, glob2rx)
    use <- c()
    for (i in 1:length(pats))
        use <- union(use, grep(pats[i], words))
    return(use)
}
