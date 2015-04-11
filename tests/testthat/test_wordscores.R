library(austin)
context("wordscores")

tol <- 0.00001

test_that("wordscores replicates the toy data example", {
  data(lbg)
  cws <- classic.wordscores(lbg[,1:5], scores=seq(-1.5, 1.5, 0.75))
  expect_is(cws, 'wordscores')
  expect_is(cws, 'classic.wordscores')
  
  (summ <- summary(cws))
  expect_equal(summ$Total, rep(1000, 5))
  expect_equal(summ$Max[1], 158)
  expect_equal(summ$Mean[1], 27.02703, tolerance=tol)
  
  (ws <- coef(cws))
  expect_equal(ws['A',], -1.5)
  expect_equal(ws['F',], -1.48125)
  expect_equal(ws['Z',], 1.0369898)
  expect_equal(ws['ZK',], 1.5)
  
  pre <- predict(cws, newdata=lbg[,6,drop=FALSE])
  expect_equal(pre$Score[1], -0.4480591, tolerance = tol)
  ## The Laver et al. paper says the SE is 0.018. That's a typo
  expect_equal(pre$'Std. Err.'[1], 0.01189767, tolerance = tol)
  expect_equal(pre$Rescaled[1], -0.4480591, tolerance = tol)
  expect_equal(pre$Lower[1], -0.4593619, tolerance = tol)
  expect_equal(pre$Upper[1], -0.4367563, tolerance = tol)
  
})

test_that("wordscores (nearly) replicates the UK manifesto data", {
  data(ukmanif)
  cws <- classic.wordscores(ukmanif[,c(2,4,5)], 
                            scores=c(17.21, 5.35, 8.21))
  
  (summ <- summary(cws))
  expect_equal(summ$Total, c(28672,11345,17203))
  expect_equal(summ$Max, c(1851,613,992))
  expect_equal(summ$Mean, c(4.007828, 1.585826, 2.404669), 
               tolerance=tol)
  
  (ws <- coef(cws))
  expect_equal(nrow(ws), 5511)
  expect_equal(ws['wide',], 9.767373, tolerance=tol)
  expect_equal(ws['travel',], 13.995690, tolerance=tol)
  expect_equal(ws['tax',], 9.932435, tolerance=tol)
  
  (pre <- predict(cws, newdata=ukmanif[,c(1,3,6)]))
  ## within 0.05 of the paper's numbers
  expect_equal(pre$Rescaled, c(9.193603, 17.160808, 4.972796),
               tolerance = tol)
  ## The Laver et al. paper says the SE is 0.018. That's a typo
  expect_equal(pre$'Std. Err.'[1], 0.01479069, tolerance = tol)
})
