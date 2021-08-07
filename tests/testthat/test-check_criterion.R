library(testthat)
library(openFHDQR)

X <- matrix(rnorm(10 * 100), nrow = 10)
bb <- c(rep(100, 10), rep(0, 90))
y <- X %*% bb

c1cpp <- check_criterion1(X = X, beta = bb, z = y - X %*% bb, y = y, eps1 = 0.001, eps2 = 0.001)
c1r <- check_criterion1R(X = X, beta = bb, z = y - X %*% bb, y = y, epsilon1 = 0.001, epsilon2 = 0.001)


test_that("check_criterion1 output has correct length & matches R output", {
  expect_equal(c1cpp, c1r)
  expect_length(c1cpp, 1)
})

c2cpp <- check_criterion2(sigma = 0.05, X = X, z = y - X %*% bb, old_z = y, eps1 = 0.001, eps2 = 0.001, theta = X %*% bb)
c2r <- check_criterion2R(sigma = 0.05, X = X, z = y - X %*% bb, old_z = y, epsilon1 = 0.001, epsilon2 = 0.001, theta = X %*% bb)

test_that("check_criterion2 output has correct length & matches R output", {
  expect_equal(c2cpp, c2r)
  expect_length(c2cpp, 1)
})

