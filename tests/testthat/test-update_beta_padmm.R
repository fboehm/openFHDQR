library(testthat)
library(openFHDQR)

X <- matrix(rnorm(10 * 100), nrow = 10)
bb <- c(rep(100, 10), rep(0, 90))
y <- X %*% bb

foo <- update_beta_padmm(beta = bb, X = X, theta = rep(1, 10), sigma = 0.05, eta = 1000, y = y, z = y, l1 = 1, l2 = 0, w = rep(1, 100), nu = rep(1, 100))
bar <- update_beta_padmmR(beta = bb, X = X, theta = rep(1, 10), sigma = 0.05, eta = 1000, y = y, z = y, l1 = 1, l2 = 0, w = rep(1, 100), nu = rep(1, 100))

test_that("shrink function works", {
  expect_equal(as.numeric(foo), bar)
  expect_length(as.numeric(foo), 100)
})


