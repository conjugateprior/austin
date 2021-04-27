# open question: should tokens keep a types attribute?

#' Create tokens using a dictionary
#'
#' Runs a dictionary on the text column of x to create 'tokens' variable 
#' consisting of dictionary matches to each word. Non-matching words
#' from the dictionary generate no token.
#'
#' @param x a tibble
#' @param dictionary a quanteda content analysis dictionary
#' @param ... extra arguments to tokenizers::tokenizer_*
#'
#' @return a tibble
#' @export
jl_tokenize_categories <- function(x, dictionary, ...){
  stopifnot("text" %in% names(x))
  res <- tokens_lookup(tokens(x[["text"]], ...),
                       dictionary = dictionary)
  voc <- sort(unique(unname(unlist(res))))
  x[["tokens"]] <- lapply(res, function(x){ as.integer(factor(x, levels = voc)) })
  attr(x[["tokens"]], "types") <- voc
  x
}


#' Get category counts from a dictionary
#'
#' Runs a dictionary on the text column of x to create 'tokens' variable 
#' consisting of dictionary matches to each word, (non-matching words
#' from the dictionary generate no token), then tabulates them within 
#' each document in 'counts' variable
#'
#' This is a shortcut for running  jl_tokenize_categories then jl_count_tokens
#' and should be used in preference to these two.
#'
#' @param x a tibble
#' @param dictionary a quanteda content analysis dictionary
#' @param ... extra arguments to tokenizers::tokenizer_*
#'
#' @return a tibble with 'tokens' and 'counts' variables
#' @export
jl_count_categories <- function(x, dictionary, ...){
  jl_tokenize_categories(x, dictionary = dictionary, ...)
  jl_count_tokens(x)
}



#' Create corpus from wide counts
#'
#' Given a tibble x of variables, some of which are wide form category counts, and 
#' the remainder of which are document variables (docvars in quanteda terminology),
#' decompose the variables specified by the tidyselect arguments into a 'counts'
#' variable in sparse format and keep the remainder.
#'   
#' @param x a tibble 
#' @param ... a tidy select expression
#' @param drop_old_vars whther to remove the wide variables (default: TRUE)
#' @param drop_unused_vars whether to drop wide variables whose column sums are zero
#'
#' @return a tibble with a 'counts' variable
#' @export
jl_process_topics <- function(x, ...,
                              drop_old_vars = TRUE,      # drop the columns?
                              drop_unused_vars = TRUE){  # drop colSum == 0?
  xx <- dplyr::select(x, ...)
  
  voc <- names(xx) # take them in column order
  ff <- function(rw) {
    nz <- which(as.numeric(xx[rw,]) > 0)
    matrix(as.integer(c(nz, xx[rw,][nz])), ncol = 2)
  }
  x[["counts"]] <- lapply(1:nrow(x), ff)
  attr(x[["counts"]], "types") <- voc
  if (drop_old_vars)
    x <- select(x, -names(xx))
  if (drop_unused_vars)
    x <- jl_reindex(x)
  x
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
  add_column(x, doc_id = 1:nrow(x), .before = 1)
}


#' Create a corpus
#'
#' Make a data frame or tibble fit for text analysis.
#'
#' @param x a data frame or tibble
#' @param text_var the name of the variable containing text
#' @param remove_old_text_vars whether to remove old text variable after renaming
#'
#' @return a tibble
#' @export
jl_incorporate <- function(x, text_var = "text",
                           remove_old_text_vars = TRUE){
  stopifnot(text_var %in% names(x))
  x <- jl_identify(x) # refresh or add a document identifier
  if (text_var != "text") {
    if ("text" %in% names(x)) {
      warning("Renaming existing 'text' variable to 'text_old'")
      x[["text_old"]] <- x[["text"]]
      x[["text"]] <- NULL
    }
    txtvar <- x[[text_var]]
    if (remove_old_text_vars)
      x[[text_var]] <- NULL
    tibble(x, text = txtvar)
  } else {
    ti <- which(names(x) == "text")
    tibble(x[,-ti], text = x[["text"]])
  }
}



#' Split a corpus into subunits
#'
#' If a document 23 of x contains 3 sentences in its 'text' then 
#' jl_split(x, "sentences")
#' returns three new rows with other variables duplicated, 
#' new 'tokens' values, and doc_ids 23.1 23.2 and 23.3
#'
#' @param x a tibble
#' @param what what unit to disaggregate a document to (default: paragraphs)
#' @param ... extra arguments to give to tokenizers::tokenize_*
#'
#' @return a tibble with new doc_id
#' @export
jl_split <- function(x, what = c("paragraphs", "sentences", "regex"), ...){
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
  
  if (what == "paragraphs")
    spls <- tokenizers::tokenize_paragraphs(x[["text"]], ...)
  else if (what == "sentences")
    spls <- tokenizers::tokenize_sentences(x[["text"]], ...)
  else if (what == "regex")
    spls <- tokenizers::tokenize_regex(x[["text"]], ...)
  if (nrow(x) == sum(lengths(spls)))
    warning("This process will not alter the number of documents!")
  
  spl_f <- function(r){
    new_doc_id <- paste0(x[["doc_id"]][r], ".", 1:length(spls[[r]]))
    tibble(doc_id = new_doc_id,
           x[r, setdiff(names(x), c("text", "doc_id"))],
           text = spls[[r]])
  }
  bind_rows(lapply(1:nrow(x), spl_f))
}


#' Reindex counts variable
#' 
#' Re indexes 'counts' and provides a new 'types' attribute for it.
#'
#' @param res a tibble with a 'counts' variable
#'
#' @return a tibble
jl_reindex <- function(res){
  voc <- attr(res[["counts"]], "types")
  new_vocab <- sort(unique(unlist(lapply(res[["counts"]],
                                         function(x) x[,1]))))
  message("Reindexing counts: dropping ",
          length(voc) - length(new_vocab),
          " types that are not used")
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


#' Filter a corpus
#'
#' Filter out some documents using dplyr expressions, reindexing 
#' 'counts' as necessary
#'
#' @param x a tibble
#' @param ... a dplyr expression specifying rows
#' @param .preserve whether to keep groups (default: FALSE)
#'
#' @return
#' @export
jl_filter <- function(x, ..., .preserve = FALSE){
  res <- dplyr::filter(x, ..., .preserve = .preserve)
  if ("counts" %in% names(res))
    jl_reindex(res)
  else
    res
}

#' Split text into words
#'
#' Fills the 'tokens' variable with word tokens.
#'
#' @param x a tibble
#' @param ... extra arguments to tokenizers::tokenize_*
#'
#' @return a tibble
#' @export
jl_tokenize_words <- function(x, ...){
  toks <- tokenizers::tokenize_words(x[["text"]], ...)
  ff <- factor(unlist(toks))
  ffv <- levels(ff)
  x[["tokens"]] <- lapply(toks,
                          function(x) as.integer(factor(x, levels = ffv)))
  attr(x[["tokens"]], "types") <- ffv
  x
}

#' Get word counts
#'
#' Splits 'text' into words as a 'tokens' variable then tabulates them 
#' within each document in 'counts' variable
#'
#' This is a shortcut for running  jl_tokenize_words then jl_count_tokens
#' and should be used in preference to these two.
#'
#' @param x a tibble
#' @param ... extra arguments to tokenizers::tokenize_*
#'
#' @return a tibble
#' @export
jl_count_words <- function(x, ...){
  toks <- tokenizers::tokenize_words(x[["text"]], ...)
  ff <- factor(unlist(toks))
  ffv <- levels(ff)
  x[["tokens"]] <- lapply(toks,
                          function(x) as.integer(factor(x, levels = ffv)))
  attr(x[["tokens"]], "types") <- ffv
  jl_count_tokens(x)
}

# for grouped data frames, summarize counts
jl_summarize_counts <- function(x){
  stopifnot(is_grouped_df(x))
  gg <- group_data(x)
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
  gg
}

jl_count_tokens <- function(x){
  stopifnot("tokens" %in% names(x))
  voc <- attr(x[["tokens"]], "types")
  n <- length(voc)
  ff <- function(x){
    cnts <- tabulate(x, n)
    nz <- which(cnts > 0)
    matrix(as.integer(c(nz, cnts[nz])), ncol = 2)
  }
  x[["counts"]] <- lapply(x[["tokens"]], ff)
  attr(x[["counts"]], "types") <- voc
  x
}

jl_promote_counts <- function(x, prefix = NULL){
  stopifnot("counts" %in% names(x))
  voc <- attr(x[["counts"]], "types")
  if (!is.null(prefix))
    voc <- paste0(prefix, voc)
  inter <- intersect(names(x), voc)
  if (length(inter != 0)) # forbid overriding
    stop(paste("'counts' vocabulary overlaps existing variables:",
               inter, "\nRemove or rename the overlap or consider setting prefix"))
  dd <- as_tibble(jl_dfm(x, inflate = TRUE))
  if (!is.null(prefix))
    colnames(dd) <- voc
  le <- min(which(names(x) %in% c("text", "tokens", "counts", "text")))
  add_column(x, dd, .before = le) # back with the non-derived non-text variables
}

jl_demote_counts <- function(x, prefix = NULL){
  voc <- attr(x[["counts"]], "types")
  if (!is.null(prefix))
    voc <- paste0(prefix, voc)
  rem <- setdiff(names(x), voc)
  x[,rem]
}

jl_types <- function(x){
  if ("counts" %in% names(x))
    attr(x[["counts"]], "types")
  else if ("tokens" %in% names(x))
    attr(x[["tokens"]], "types")
  else
    stop("neither 'counts' nor 'tokens' exist")
}

jl_doclen <- function(x){
  if ("counts" %in% names(x))
    vapply(x[["counts"]], function(x) sum(x[,2]), FUN.VALUE = c(0L))
  else if ("tokens" %in% names(x))
    vapply(x[["tokens"]], function(x) length(x), FUN.VALUE = c(0L))
  else
    stop("neither 'counts' nor 'tokens' exist")
}

jl_dfm <- function(x, rownames_from = "doc_id",
                   inflate = FALSE){
  stopifnot("counts" %in% names(x))
  j <- lapply(x[["counts"]], function(x) x[,1])
  v <- lapply(x[["counts"]], function(x) x[,2])
  rws <- NULL
  if (!is.null(rownames_from))
    rws <- x[[rownames_from]]
  cls <- jl_types(x)
  m <- sparseMatrix(rep(1:nrow(x), lengths(j)),
                    j = unlist(j),
                    x = unlist(v),
                    dimnames = list(doc = rws, feat = cls))
  if (inflate)
    m <- as.matrix(m)
  m
}
