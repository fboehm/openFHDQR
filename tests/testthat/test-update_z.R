library(testthat)
library(openFHDQR)

X <- matrix(rnorm(10 * 100), nrow = 10)
bb <- c(rep(100, 10), rep(0, 90))
y <- X %*% bb

foo <- update_z(y = y, X = X, beta = bb, theta = rep(1, 10), sigma = 0.05, tau = 0.05)
bar <- update_zR(y = y, X = X, beta = bb, theta = rep(1, 10), sigma = 0.05, tau = 0.05)

test_that("shrink function works", {
  expect_equal(as.numeric(foo), bar)
  expect_length(as.numeric(foo), 10)
})


