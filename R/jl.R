# open question: should tokens keep a types attribute?

# make tokens from: 
#  1. text
#  3. text and a dictionary
#
# make counts from:
#  1. tokens
#  2. variables treated as counts

# design docs
# 
# we are not in the business of tokenizing, just storing and moving tokens
#   and generating counts
# the fundamental data structure is a tibble with some optional list columns
# structure: a jl_df contains a 
#   doc_id - a unique identifier 
#   tokens - a list column containing character vectors
#   counts - a list column containing matrices and an attribute with the types list
#
# use cases:
#
# 1. take a data frame with a text column and turn it into a jl_df
#   with a tokens column
# 2. take a data frame with a text column and a dictionary and turn it into a jl_df 
#   with a tokens column
# 3. take a data frame or jl_df with a tokens column and turn it into a jl_df 
#   with a counts column+attr
# 4. take a data frame with some variables that are counts in wide form and 
#   turn it into a jl_df with a counts column+attr (CMP no tokens)
# 5. take a jl_df with a counts column+attr and turn it into a document feature 
#   matrix with rownames, colnames, and sparsity option
# 6. take a jl_df with a text column and (optionally) a counts column+attr and 
#   break up its a textual unit, e.g. paragraphs, sentences, etc. (split / disaggregate)
# 7. take a jl_df and remove documents using a variable expression (filter)
# 8. take a grouped jl_df with a counts variable and collapse the counts into 
#   a new jl_df (summarize)
# 9. Surface counts as new variables in a jl_df
# 10. Submerge counts surfaced from a counts variable
# 11. Get token count per document as a vector from a jl_df
# 12. filter out documents by facts about counts, e.g. minimum length


ensure_jl_df <- function(x){
  stopifnot(is.data.frame(x))
  if (is.data.frame(x) && !tibble::is_tibble(x))
    x <- tibble::as_tibble(x)
  if (!("jl_df" %in% class(x)))
    class(x) <- c("jl_df", class(x))
  x
}

#' Split each text into tokens
#'
#' Fills the 'tokens' variable with tokens. Tokens can be any strings, although 
#' in the usual cases are words or category/topic labels from a content 
#' analysis dictionary.
#' 
#' Examples:
#' 
#' To tokenize words, leave the tokenizer function at its default and add
#' and extra arguments, e.g. strip_numeric = TRUE, in the ...
#' 
#' To tokenize into category/topic labels using quanteda's resources 
#' set the tokenizer function to (here 
#' assuming you have the quanteda library loaded - if not, preface every function 
#' name with quanteda::)
#' 
#' dict_tok <- function(txt){ 
#'   tokens_lookup(tokens(txt, remove_punct = TRUE),
#'                        dictionary = mydictionary)
#' }
#' 
#' and run as jl_tokenize(mydf, tokenizer = dict_tok)
#'
#' This function adds a list column called 'tokens' to contain the tokens
#'
#' @param x a tibble
#' @param tokenizer a function that returns a list of character vectors containing tokens
#' @param ... extra arguments to 'tokenizer'
#'
#' @return jl_df
#' @export
jl_tokenize <- function(x, tokenizer = tokenizers::tokenize_words, ...){
  toks <- tokenizer(x[["text"]], ...)
  ff <- factor(unlist(toks))
  ffv <- levels(ff)
  x[["tokens"]] <- lapply(toks,
                          function(x) as.integer(factor(x, levels = ffv)))
  #attr(x[["tokens"]], "types") <- ffv
  ensure_jl_df(x)
}

#' Get the set of types
#'
#' Returns the list of types that are counted in 'counts'. If 
#' 'counts' does not exist, does the calculation for 'tokens' on the fly
#'
#' @param x a tibble
#'
#' @return a vector of type values
#' @export
jl_types <- function(x){
  if ("counts" %in% names(x))
    attr(x[["counts"]], "types")
  else if ("tokens" %in% names(x))
    levels(unique(unlist(x[["tokens"]])))
  else
    stop("neither 'counts' nor 'tokens' variables exist")
}

#' Tabulate tokens
#' 
#' When ... is unused the tokens variable is turned into a counts variable 
#' containing sparse matrices of 
#' table counts, one per document. 
#' 
#' When ... contains tidy-select expressions 
#' denoting a column range within x these columns are treated as 'wide' form
#' counts of categories/topics named by the variables. Note, counts are 
#' required and the function will break if real numbers are present. 
#' 
#' If there is anything in ... and drop_old_vars is 
#' TRUE (the default) then a new 'counts' replace all everything mentioned in 
#' the ... expression . 
#' When drop_unused_vars is TRUE (the default), if a category/topic records
#' a zero count in every row, this level is dropped. 
#' 
#' The ... case is designed for folding existing cross-tabulated data into 
#' a jl_df, e.g. hand coded data distributed by the CMP. (Note that in this 
#' particular case the data will have to be reinflated to counts). 
#'
#' @param x a jl_df with a 'tokens' variable
#' @param ... tidy-select expressions denoting variables in x
#' @param drop_old_vars whether to drop variables denoted in in ... (default: TRUE)
#' @param drop_unused_vars whether to unused token levels (default: TRUE)
#'
#' @return a jl_df with a 'counts' variable
#' @export
jl_count <- function(x, ..., 
                     drop_old_vars = TRUE,      
                     drop_unused_vars = TRUE){
  if (length(list(...)) == 0) {
    stopifnot("tokens" %in% names(x))
    voc <- levels(unique(unlist(x[["tokens"]])))
    n <- length(voc)
    ff1 <- function(x){
      cnts <- tabulate(x, n)
      nz <- which(cnts > 0)
      matrix(as.integer(c(nz, cnts[nz])), ncol = 2)
    }
    x[["counts"]] <- lapply(x[["tokens"]], ff1)
    attr(x[["counts"]], "types") <- voc
  } else {
    xx <- dplyr::select(x, ...)
    voc <- names(xx) # take them in column order
    ff2 <- function(rw) {
      nz <- which(as.numeric(xx[rw,]) > 0)
      matrix(as.integer(c(nz, xx[rw,][nz])), ncol = 2)
    }
    x[["counts"]] <- lapply(1:nrow(x), ff2)
    attr(x[["counts"]], "types") <- voc
    if (drop_old_vars)
      x <- dplyr::select(x, -names(xx))
    if (drop_unused_vars)
      x <- jl_reindex(x)
  }
  ensure_jl_df(x)
}

#' Add a document identifier
#'
#' Adds a variable 'doc_id' to uniquely identify each row. This identifier
#' may later, if jl_split is used, contain information about disaggregation 
#' level. If the document identifier already exists, overwrite it.
#'
#' @param x a tibble
#'
#' @return a tibble
#' @export
jl_identify <- function(x){
  if ("doc_id" %in% names(x)) {
    warning("Removing existing 'doc_id'")
    x[["doc_id"]] <- NULL
  }
  tt <- tibble::add_column(x, doc_id = 1:nrow(x), .before = 1)
  ensure_jl_df(tt)
}

#' Divide each texts
#'
#' This function inflates the jl_df by 'exploding' each text using the 
#' tokenizer function and duplicating the non-textual variables across
#' the new elements. By default tokenizer choice this function makes 
#' each paragraph a new document. Replace this function with your prefered 
#' splitting function. It should return a list of character vectors. To 
#' add extra arguments to the tokenizer function call, put them in ...
#'
#' @param x a tibble
#' @param tokenizer a function that splits each text
#' @param ... extra arguments to give to tokenizer
#'
#' @return a tibble with new doc_id
#' @export
jl_inflate <- function(x, tokenizer = tokenizers::tokenize_paragraphs, ...){
  stopifnot("text" %in% names(x))
  what <- match.arg(what)
  if ("tokens" %in% names(x)) {
    warning("Dropping existing 'tokens' variable")
    x[["tokens"]] <- NULL
  }
  if ("counts" %in% names(x)) {
    warning("Dropping existing 'counts' variable")
    x[["counts"]] <- NULL
  }
  
  spls <- tokenizer(x[["text"]], ...)
  spl_f <- function(r){
    new_doc_id <- paste0(x[["doc_id"]][r], ".", 1:length(spls[[r]]))
    dplyr::tibble(doc_id = new_doc_id,
                  x[r, setdiff(names(x), c("text", "doc_id"))],
                  text = spls[[r]])
  }
  x <- dplyr::bind_rows(lapply(1:nrow(x), spl_f))
  ensure_jl_df(x)
}


#' Reindex the counts variable
#' 
#' Re indexes 'counts' and provides a new 'types' attribute for it.
#' You shouldn't have to call this yourself.
#'
#' @param res a jl_df
#'
#' @return a tibble
jl_reindex <- function(res){
  voc <- attr(res[["counts"]], "types")
  new_vocab <- sort(unique(unlist(lapply(res[["counts"]],
                                         function(x) x[,1]))))
  if (length(voc) == length(new_vocab)) # check this is always safe!
    return(res)
  
  message("Reindexing counts")
  if (length(voc) - length(new_vocab) != 0)
    message("Dropping ", length(voc) - length(new_vocab), " types that are not used")
  tr <- integer(length(voc))
  tr[new_vocab] <- 1:length(new_vocab) # 1 -> 1, 2 (gone) -> 2, 3 -> 2
  res[["counts"]] <- lapply(res[["counts"]],
                            function(cnt){
                              for (i in 1:nrow(cnt))
                                cnt[i, 1] <- tr[cnt[i, 1]]
                              cnt
                            })
  attr(res[["counts"]], "types") <- voc[new_vocab]
  res
}


#' Filter by document variable
#'
#' Filter documents using logical expressions, as you would in dplyr. This 
#' function will re-index 'counts' as necessary
#'
#' @param x a jl_df
#' @param ... a logical expression specifying rows
#' @param .preserve whether to keep groups (default: FALSE)
#'
#' @return a jl_df
#' @export
jl_filter <- function(x, ..., .preserve = FALSE){
  res <- dplyr::filter(x, ..., .preserve = .preserve)
  if ("counts" %in% names(res))
    res <- jl_reindex(res)
  ensure_jl_df(res)
}

#' Collapse counts to groups
#'
#' If a jl_df is grouped, collapse the 'counts' within each group. Like 
#' summarize, after group_by, but with no choice of function.
#'
#' @param x a grouped jl_df
#'
#' @return an jl_df
#' @export
jl_collapse <- function(x){
  if (!dplyr::is_grouped_df(x))
    stop("This is object is not grouped. Perhaps you were looking for jl_dfm?")
  gg <- dplyr::group_data(x)
  n <- length(jl_types(x))
  fn <- function(grp_i) {
    bl <- integer(n)
    for (cn in x[["counts"]][grp_i])
      bl[cn[,1]] <- bl[cn[,1]] + cn[,2]
    nz <- which(bl > 0)
    matrix(as.integer(c(nz, bl[nz])), ncol = 2)
  }
  gg[["counts"]] <- lapply(gg[[".rows"]], fn)
  attr(gg[["counts"]], "types") <- jl_types(x)
  gg[[".rows"]] <- NULL
  gg <- tibble::add_column(gg, doc_id = 1:nrow(gg), .before = 1)
  ensure_jl_df(gg)
}


#' Make counts full variables in wide form
#'
#' @param x a jl_df with 'counts'
#' @param prefix what to prefix each counted element's value when it turns into a variable name
#'
#' @return a jl_df
#' @export
jl_promote_counts <- function(x, prefix = NULL){
  stopifnot("counts" %in% names(x))
  voc <- attr(x[["counts"]], "types")
  if (!is.null(prefix))
    voc <- paste0(prefix, voc)
  inter <- intersect(names(x), voc)
  if (length(inter != 0)) # forbid overriding
    stop(paste("'counts' vocabulary overlaps existing variables:",
               inter, "\nRemove or rename the overlap or consider setting prefix"))
  dd <- dplyr::as_tibble(jl_dfm(x, inflate = TRUE))
  if (!is.null(prefix))
    colnames(dd) <- voc
  le <- min(which(names(x) %in% c("text", "tokens", "counts", "text")))
  x <- tibble::add_column(x, dd, .before = le) # back with the non-derived non-text variables
  ensure_jl_df(x)
}

#' Undo the effects of jl_promote_counts
#'
#' Removes the variables that jl_promote_counts added
#' 
#' @param x a tibble
#' @param prefix whether to look for a prefix when removing
#'
#' @return a tibble
#' @export
jl_demote_counts <- function(x, prefix = NULL){
  voc <- attr(x[["counts"]], "types")
  if (!is.null(prefix))
    voc <- paste0(prefix, voc)
  rem <- setdiff(names(x), voc)
  ensure_jl_df(x[,rem])
}

#' Get document lengths in tokens
#'
#' For each document, how many tokens it contains. Note that this may 
#' not correspond to the number of words or dictionary categories
#'
#' @param x a tibble
#'
#' @return a vector of token counts
#' @export
jl_lengths <- function(x){
  if ("counts" %in% names(x))
    vapply(x[["counts"]], function(x) sum(x[,2]), FUN.VALUE = c(0L))
  else if ("tokens" %in% names(x))
    vapply(x[["tokens"]], function(x) length(x), FUN.VALUE = c(0L))
  else
    stop("neither 'counts' nor 'tokens' exist")
}

#' Extract a document feature matrix
#' 
#' Throws away all variables and constructs a documnt feature 
#' matrix with column names from the types of whatever is counted in 'counts'
#' ans row names from a chosen variable, or doc_id.
#' 
#' Note that requesting an inflated document feature matrix turns a 
#' Matrix::sparseMatrix into a regular base::matrix and may increase 
#' memory consumption considerably.
#' 
#' @param x a tibble
#' @param rownames_from which variable to take the row names from (default: doc_id) 
#' @param inflate whether to return a dense base::matrix (TRUE) or a Matrix::sparseMatrix
#'
#' @return a matrix
#' @export
jl_dfm <- function(x, rownames_from = "doc_id",
                   inflate = FALSE){
  stopifnot("counts" %in% names(x))
  j <- lapply(x[["counts"]], function(x) x[,1])
  v <- lapply(x[["counts"]], function(x) x[,2])
  rws <- NULL
  if (!is.null(rownames_from))
    rws <- x[[rownames_from]]
  cls <- jl_types(x)
  m <- Matrix::sparseMatrix(rep(1:nrow(x), lengths(j)),
                            j = unlist(j),
                            x = unlist(v),
                            dimnames = list(doc = rws, feat = cls))
  if (inflate)
    m <- as.matrix(m)
  m
}

## for transforming the existing data sets, use
# demanif <- data.frame(t(demanif)) %>%
#   rownames_to_column %>%
#   extract(rowname, into = c("party", "year", "type"), 
#           regex = "t([A-Z]+)(\\d\\d\\d\\d)(.*)") %>% 
#   jl_count(4:ncol(.))
