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
# structure: a jl_df contains either or both of
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

# helper
has <- function(x, which){
  any(which == names(x))
}
stop_if_no <- function(x, which, message = NULL){ 
  if (!has(x, which)) {
    if (!is.null(message))
      stop(message)
    else
      stop("Object has no '", which, "' variable")
  }
}



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
  if (has(x, "counts"))
    attr(x[["counts"]], "types")
  else if (has(x, "tokens"))
    levels(unique(unlist(x[["tokens"]])))
  else
    stop("neither 'counts' nor 'tokens' variables exist")
}

#' Tabulate tokens
#' 
#' The 'tokens' variable is turned into a 'counts' variable 
#' containing sparse matrices of 
#' table counts, one per document. Any 'tokens' variable is maintained
#' unless drop_tokens_after is TRUE.
#'
#' @param x a jl_df object
#' @param drop_tokens_after whether to drop 'tokens' variable after 'counts' computation
#'
#' @return a jl_df with a 'counts' variable
#' @export
jl_count <- function(x, drop_tokens_after = FALSE){
  stop_if_no(x, "tokens")
  voc <- levels(unique(unlist(x[["tokens"]])))
  n <- length(voc)
  ff1 <- function(x){
    cnts <- tabulate(x, n)
    nz <- which(cnts > 0)
    matrix(as.integer(c(nz, cnts[nz])), ncol = 2)
  }
  x[["counts"]] <- lapply(x[["tokens"]], ff1)
  attr(x[["counts"]], "types") <- voc
  ensure_jl_df(x)
}

#' Construct counts from existing variables
#' 
#' When tidy-select expressions are present in ...
#' these columns are treated as counts in 'wide' form. Their counts are 
#' folded into the 'counts' variable and then removed.
#' 
#' If drop_unused_vars is TRUE (the default), if a category/topic records
#' a zero count in every row, this level is dropped. 
#' 
#' This function is designed for folding existing cross-tabulated data into 
#' a jl_df, i.e. when the text or tokens are no longer available. A prominent 
#' example is hand coded data distributed by the CMP. (In this 
#' particular case the data will also have to be re-inflated from percentages to 
#' counts beforehand). 
#'
#' Notes:
#' 
#' Variables refered to must contain integers or be coercible 
#' into integers without loss as they will be used for indexing. 
#' 
#' Any existing 
#' 'tokens' variable will be removed to ensure that there
#' is no mismatch between 'tokens' and 'counts'.
#'
#' @param x a jl_df object
#' @param ... tidy-select expressions denoting variables in x
#' @param drop_unused_vars whether to unused token levels (default: TRUE)
#'
#' @return a jl_df with a 'counts' variable
#' @export
jl_count_from_vars <- function(x, ..., 
                               drop_unused_vars = TRUE){ 
  if (has(x, "tokens")) {
    message("Removing existing 'tokens' variable")
    x[["tokens"]] <- NULL
  }
  xx <- dplyr::select(x, ...)
  voc <- names(xx) # take them in column order
  ff2 <- function(rw) {
    nz <- which(as.numeric(xx[rw,]) > 0)
    matrix(as.integer(c(nz, xx[rw,][nz])), ncol = 2)
  }
  cts <- lapply(1:nrow(x), ff2)
  x <- dplyr::select(x, -names(xx)) 
  x[["counts"]] <- cts
  attr(x[["counts"]], "types") <- voc
  if (drop_unused_vars)
    x <- jl_reindex(x)
  ensure_jl_df(x)
}

#' Add a document identifier
#'
#' Adds a variable 'doc_id' to uniquely identify each row. This identifier
#' may later, if jl_expand is used, this will be augmented with information 
#' about disaggregation level. 
#'
#' Note: v may be the name of a variable or an entirely new character variable
#' of the appropriate length and uniqueness.
#'
#' @param x a data.frame 
#' @param v variable to be used as a unique identifier (Default: one will be constructed)
#' @param drop_original whether to remove v after it is assigned to 'doc_id'
#'
#' @return a jl_df
#' @export
jl_identify <- function(x, v = NULL, drop_original = TRUE){
  if (is.null(v)) {
    if (has(x, "doc_id")) {
      message("Overwriting existing 'doc_id'")
      x[["doc_id"]] <- as.character(1:nrow(x))
    } else {
      x <- tibble::add_column(x, doc_id = as.character(1:nrow(x)), .before = 1)
    }
    return(x)
  }
    
  check_length <- function(z){ 
    if (length(z) != nrow(x))
      stop("Proposed doc_id is not the same length as the number of rows")
  }
  check_uniqueness <- function(z){
    if (length(unique(z)) != nrow(x))
      stop("Proposed doc_id is not a unique identifier")
  }
  
  if (length(v) > 1) { # they are handing us a whole vector 
    check_length(v)
    check_uniqueness(v)
    x <- tibble::add_column(x, doc_id = v, .before = 1)
  } else { 
    # they are handing us a variable name
    check_length(x[[v]])
    check_uniqueness(x[[v]])
    if (has(x, "doc_id")) {
      message("Overwriting existing 'doc_id'")
      x[["doc_id"]] <- as.character(1:nrow(x))
    } else {
      x <- tibble::add_column(x, doc_id = as.character(x[[v]]), .before = 1)
    }
    if (drop_original && v != "doc_id")
      x[[v]] <- NULL
  }
  x
}


#' Split each text into new ones
#'
#' This function inflates the jl_df by 'exploding' each text using the 
#' tokenizer function and duplicating the non-textual variables across
#' the new elements. 
#' 
#' By default tokenizer choice this function makes 
#' each paragraph a new document. Replace this function with your preferred 
#' splitting function. It should return a list of character vectors. To 
#' add extra arguments to the tokenizer function call, put them in ...
#'
#' If 'doc_id' is present it is augmented to show the nesting.
#'
#' @param x a tibble
#' @param tokenizer a function that splits each text
#' @param ... extra arguments to give to tokenizer
#'
#' @return a tibble with new doc_id
#' @export
jl_expand <- function(x, tokenizer = tokenizers::tokenize_paragraphs, ...){
  stop_if_no(x, "text")
  if (has(x, "tokens")) {
    warning("Dropping existing 'tokens' variable")
    x[["tokens"]] <- NULL
  }
  if (has(x, "counts")) { 
    warning("Dropping existing 'counts' variable")
    x[["counts"]] <- NULL
  }
  
  spls <- tokenizer(x[["text"]], ...)
  spl_f <- function(r){
    nms <- setdiff(names(x), c("text", "doc_id"))
    xx <- dplyr::tibble(x[r, nms], text = spls[[r]]) # text at the end
    if (has(x, "doc_id")) # and new doc_id if needed
      xx[["doc_id"]] <- paste0(x[["doc_id"]][r], ".", 1:length(spls[[r]]))
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
  if (has(x, "counts")) 
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
  stop_if_no(x, "counts")
  voc <- attr(x[["counts"]], "types")
  if (!is.null(prefix))
    voc <- paste0(prefix, voc)
  inter <- intersect(names(x), voc)
  if (length(inter != 0)) # forbid overriding
    stop("'counts' vocabulary overlaps existing variables: [",
         paste(inter, collapse = ","), 
         "] Maybe set a prefix?")
  dd <- dplyr::as_tibble(jl_dfm(x, sparse = FALSE))
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
  if (has(x, "counts"))
    vapply(x[["counts"]], function(x) sum(x[,2]), FUN.VALUE = c(0L))
  else if (has(x, "tokens"))
    vapply(x[["tokens"]], function(x) length(x), FUN.VALUE = c(0L))
  else
    stop("neither 'counts' nor 'tokens' exist")
}

#' Get type counts for tokens
#'
#' This assumes that a 'counts' variable exists. Use jl_count to create it
#' if necessary.
#'
#' @param x a jl_df object
#'
#' @return a vector of type counts
#' @export
jl_freqs <- function(x){
  stop_if_no(x, "counts")
  tps <- jl_types(x)
  fr <- integer(length(tps))
  cnts <- x[["counts"]]
  for (i in 1:nrow(x)) {
    m <- cnts[[i]]
    fr[m[, 1]] <- fr[m[, 1]] + m[, 2] 
  }
  names(fr) <- tps
  fr
}

#' Extract a document feature matrix
#' 
#' Constructs a document feature 
#' matrix using 'counts' with column names taken from the types attribute 
#' of 'counts' and row names from 'doc_id' or a user provided variable.
#'  
#' If sparse = FALSE (the default) then the result is a CsparseMatrix from the 
#' Matrix package and if TRUE, a dense base::matrix. Note that Non-sparse 
#' document feature matrices may become quite large.
#' 
#' @param x a tibble
#' @param rownames_from column of x to use as rownames
#' @param sparse whether to return a sparse (the default) or dense matrix 
#'
#' @return Matrix::dgCMatrix (or base::matrix if sparse = FALSE)
#' @export
#' 
#' @example 
#' data(LBG2003)
#' jl_dfm(LBG2003, rownames_from = "doc_id")
#' 
jl_dfm <- function(x, rownames_from = NULL, sparse = TRUE){
  stop_if_no(x, "counts")
  j <- lapply(x[["counts"]], function(x) x[,1])
  v <- lapply(x[["counts"]], function(x) x[,2])
  rws <- NULL
  if (!is.null(rownames_from))
    rws <- as.character(x[[rownames_from]])
  cls <- jl_types(x)
  m <- Matrix::sparseMatrix(rep(1:nrow(x), lengths(j)),
                            j = unlist(j),
                            x = unlist(v),
                            dimnames = list(docs = rws, words = cls))
  if (!sparse)
    m <- as.matrix(m)
  m
}

is_sparse <- function(x){
  is(x, "sparseMatrix")
}

############# models

#' Compute Positions via Reciprocal Averaging
#'
#' This function runs the simplest form of reciprocal averaging and produces
#' estaimted theta (document) and beta (word) positions. Theta is mean zero 
#' and unit variance. Beta is mean zero.
#' 
#' Why would you use this function? Perhaps because it is like doing 
#' 1-dimensional correspondence analysis but never computes a full SVD
#' and never stores more than one copy of the input data, which can be 
#' sparse. Hence it saves memory. 
#' 
#' Why would you not use this function? Because it trades space for time. 
#' It's usually quicker to run SVD on the (dense) residuals matrixes as 
#' regular correspondence is defined as doing. Also this function will not get 
#' recover than one set of document and feature positions.
#'
#' @param x a matrix 
#' @param dir Require that document positions are such that dir[1] < dir[2]
#' @param tol the largest position difference small enough to stop iterating
#' @param verbose whether to report iterations and differences
#' @param iter.max maximum number of iterations regardless of tol
#'
#' @return a list of theta and beta estimates
#' @export unit mean zero unit standard deviation scores for documents and features
#' @examples
#' 
#' data(LBG2003)
#' lbg_dfm <- jl_dfm(LBG2003, rownames_from = "doc_id"))
#' res <- jl_reciprocal_average(lbg_dfm)
#' res$theta # the six document scores
jl_reciprocal_average <- function(x, dir = c(1,nrow(x)), 
                                  tol = 0.001, verbose = FALSE, iter.max = 50){
  fix_scale <- function(x){
    m <- sum(x) / length(x)
    s <- sqrt(sum((x - m)^2) / length(x))
    (x - m) / s
  }
  col_n <- colSums(x)
  row_n <- rowSums(x)
  theta <- fix_scale(rnorm(length(row_n)))
  beta <- fix_scale(rnorm(length(col_n)))
  theta <- fix_scale(theta)
  if (theta[dir[1]] > theta[dir[2]])
    theta <- theta * (-1)

  old_theta <- theta + 0.1
  i <- 0
  while (1) {
    d <- max(old_theta - theta)
    if (d < tol) {
      message("max(old_theta - theta) < ", tol, " after ", i, " iterations\n")
      break
    }
    if (i > iter.max) {
      message("Iterations exceeded ", iter.max, "\n")
      break
    }
    if (verbose)
      cat(i, ": tol = ", d, "\n")
    
    old_theta <- theta
    theta <- as.vector(sweep(x, MARGIN = 1, STATS = row_n, FUN = `/`) %*% beta)
    theta <- fix_scale(theta)
    beta <- as.vector(t(sweep(x, MARGIN = 2, STATS = col_n, FUN = `/`)) %*% theta)
    beta <- fix_scale(beta)
    i <- i + 1
  }
  # fix direction
  if (theta[dir[1]] > theta[dir[2]]) {
    theta <- theta * (-1)
    beta <- beta * (-1)
  }
  list(theta = theta, beta = beta)
}

fix_scale <- function(x){
  m <- sum(x) / length(x)
  s <- sqrt(sum((x - m)^2) / length(x))
  (x - m) / s
}

pop_sd <- function(x) {
  sqrt(sd(x)^2 * (length(x) - 1) / length(x))
}

lbg_maker <- function(n = 10, gap = 2){
  v <- c(2, 3, 10, 22, 45, 78, 115, 146, 158, 146, 115, 78, 45, 22, 10, 3, 2)
  vlen <- length(v)
  cols <- vlen + (n-1)*gap
  res <- lapply(1:n, function(g){ 
    vv <- integer(cols)
    st <- 1 + (g-1)*gap
    vv[st:(st+vlen-1)] <- v 
    vv
  })
  m <- Matrix::Matrix(as.matrix(do.call(rbind, res)), sparse = TRUE)
  dimnames(m) <- list(doc = paste0("D",
                                   1:nrow(m)),
                                   feat = paste0("W", 1:ncol(m)))
  m
}

#' Compute Positions via Reciprocal Averaging
#'
#' This function runs the simplest form of reciprocal averaging and produces
#' estaimted theta (document) and beta (word) positions. Theta is mean zero 
#' and unit variance. Beta is mean zero.
#' 
#' Why would you use this function? Perhaps because it is like doing 
#' 1-dimensional correspondence analysis but never computes a full SVD
#' and never stores more than one copy of the input data, which can be 
#' sparse. Hence it saves memory. 
#' 
#' Why would you not use this function? Because it trades space for time. 
#' It's usually quicker to run SVD on the (dense) residuals matrixes as 
#' regular correspondence is defined as doing. Also this function will not get 
#' recover than one set of document and feature positions.
#'
#' @param x a matrix 
#' @param masked_theta fixed and estimated values for theta
#' @param dir Require that document positions are such that dir[1] < dir[2]
#' @param tol the largest position difference small enough to stop iterating
#' @param verbose whether to report iterations and differences
#' @param iter.max maximum number of iterations regardless of tol
#'
#' @return a list of theta and beta estimates
#' @export unit mean zero unit standard deviation scores for documents and features
#' @examples
#' 
#' data(LBG2003)
#' lbg_dfm <- jl_dfm(LBG2003, rownames_from = "doc_id"))
#' res <- jl_reciprocal_average(lbg_dfm)
#' res$theta # the six document scores
jl_reciprocal_average_masked <- function(x, masked_theta,
                                         dir = c(1,nrow(x)), 
                                         tol = 0.001, 
                                         verbose = FALSE, 
                                         iter.max = 50){
  fixed <- which(!is.na(masked_theta))
  free <- which(is.na(masked_theta))

  col_n <- colSums(x)
  row_n <- rowSums(x)
  theta <- masked_theta
  theta[free] <- rnorm(length(free), 
                       mean = mean(theta[fixed]), sd = sd(theta[fixed]))
  theta <- fix_scale(theta)
  beta <- fix_scale(rnorm(length(col_n)))
  
  # |fixed| > 2 should over constrain so this is not needed
  #if (theta[dir[1]] > theta[dir[2]])
  #  theta <- theta * (-1)
  #
  old_theta <- theta + 0.1
  i <- 0
  while (1) {
    d <- max(old_theta[free] - theta[free])
    if (d < tol) {
      message("max(old_theta[free] - theta[free]) < ", tol, " after ", i, " iterations\n")
      break
    }
    if (i > iter.max) {
      message("Iterations exceeded ", iter.max, "\n")
      break
    }
    if (verbose)
      cat(i, ": tol = ", d, "\n")
    
    old_theta <- theta
    theta[free] <- as.vector(sweep(x, MARGIN = 1, STATS = row_n, FUN = `/`) %*% beta)[free]
    print(theta[free])
    theta <- fix_scale(theta)
    beta <- as.vector(t(sweep(x, MARGIN = 2, STATS = col_n, FUN = `/`)) %*% theta)
    beta <- fix_scale(beta)
    i <- i + 1
  }
  # fix direction
  #if (theta[dir[1]] > theta[dir[2]]) {
  #  theta <- theta * (-1)
  #  beta <- beta * (-1)
  #}
  list(theta = theta, beta = beta)
}

