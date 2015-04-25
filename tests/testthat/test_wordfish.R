library(austin)
context("wordfish")

tol <- 0.00001

test_that("wordfish replicates the generated data", {
  set.seed(1234)
  dd <- sim.wordfish()
  wmod <- wordfish(dd$Y, dir=c(1,10))
  expect_is(wmod, 'wordfish')
  expect_true(cor(wmod$beta, dd$beta) > 0.99)
  expect_true(cor(wmod$theta, dd$theta) > 0.99)
})
