library(testthat)
library(openFHDQR)



test_that("prox function works", {
  expect_equal(prox(1.5, 0.3, 0.5), proxR(1.5, 0.3, 0.5))
  expect_length(prox(1.5, 0.3, 0.5), 1)
})


