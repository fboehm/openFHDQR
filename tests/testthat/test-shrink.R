library(testthat)
library(openFHDQR)



test_that("shrink function works", {
  expect_equal(shrink(1, 2), shrinkR(1, 2))
  expect_length(shrink(1, 2), 1)
})


