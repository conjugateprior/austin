library(dplyr)
library(austin)
context("jl")

data("LG2000")

test_that("jl_filter", {
  refs <- jl_filter(LG2000, party == "Con")
  expect_true(nrow(refs) == 2)
  expect_is(refs, "jl_df")
  expect_length(jl_types(refs), 4898)
})

test_that("jl_collapse", {
  gr <- group_by(LG2000, party)
  grs <- jl_collapse(gr)
  expect_is(grs, "jl_df")
  # we did nt lose types
  expect_identical(jl_types(grs), jl_types(LG2000))
  # or counts
  expect_identical(jl_freqs(grs), jl_freqs(LG2000))
  ll <- jl_lengths(LG2000)
  expect_identical(ll[1] + ll[2], jl_lengths(grs)[1])
})

test_that("jl_promote and _demote", {
  refs <- jl_filter(LG2000, party == "Con")
  expect_error(jl_promote_counts(refs))
  prom <- jl_promote_counts(refs, prefix = "w_")
  expect_equal(dim(prom), c(2, 4903))

  dprom <- jl_demote_counts(prom, prefix = "w_")
  expect_equal(dim(dprom), c(2, 5))
  expect_identical(dprom, refs)
})

test_that("jl_dfm", {
  refs <- jl_filter(LG2000, party == "Con")
  refs_d <- jl_dfm(refs)
  expect_equal(dim(refs_d), c(2, 4898))
  expect_identical(jl_types(refs), colnames(refs_d))
  expect_identical(refs[["doc_id"]], rownames(refs_d))
})

test_that("jl_expand", {})
          
test_that("jl_lengths", {})

test_that("jl_freqs", {})

test_that("jl_types", {})

test_that("jl_counts", {})

test_that("jl_counts_from_vars", {})

