#' Pull Words From a List
#' 
#' Extract a list of matching words from another list of words
#' 
#' 
#' @param words the words against which patters are matched
#' @param patternfile file containing the patters to match, one per line
#' @param pattern.type marks whether the patterns are 'globs' or full regular
#' expressions
#' @return A list of matching words.
#' @importFrom utils read.table glob2rx
#' @export
#' @author Will Lowe
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
