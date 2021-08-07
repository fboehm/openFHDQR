library(testthat)

test_that("calc_chi works", {
  expect_equal(calc_chii(beta = rep(0, 5), index = 1, X = matrix(nrow = 10, ncol = 5, data = rnorm(50)), theta = rep(1, 10), sigma = 1, y = rep(10, 10), z = rep(0, 10)), 11)
})


