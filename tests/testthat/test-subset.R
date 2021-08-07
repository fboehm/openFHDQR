library(testthat)

X <- matrix(rnorm(10 * 100), nrow = 10)
bb <- c(rep(100, 10), rep(0, 90))
y <- X %*% bb



test_that("subset functions work", {
  expect_equal(X[, 2, drop = FALSE], choose_col(X, 2 - 1))
  expect_equal(X[1, , drop = FALSE], choose_row(X, 1 - 1))
  expect_equal(X[2, 3, drop = FALSE], choose_element(X, 2-1, 3-1))
})


