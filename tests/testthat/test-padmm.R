library(testthat)

X <- matrix(rnorm(10 * 100), nrow = 10)
bb <- c(rep(100, 10), rep(0, 90))
y <- X %*% bb

foo <- padmm(beta0 = rep(1, 100),
                 z0 = X %*% bb - y,
                 theta0 = rep(1, 10),
                 sigma = 0.05, X = X,
                 eta = 100,
                 y = y,
                 l1 = 0.1,
                 l2 = 0,
                 w = rep(1, 100),
                 nu = rep(1, 100),
                 tau = 0.5,
                 gamma = 1,
                 max_iter = 1000,
                 eps1 = 0.001,
                 eps2 = 0.001)
bar <- padmmR(beta0 = rep(1, 100),
                 z0 = X %*% bb - y,
                 theta0 = rep(1, 10),
                 sigma = 0.05, X = X,
                 eta = 100,
                 y = y,
                 l1 = 0.1,
                 l2 = 0,
                 w = rep(1, 100),
                 nu = rep(1, 100),
                 tau = 0.5,
                 gamma = 1,
                 max_iter = 1000,
                 epsilon1 = 0.001,
                 epsilon2 = 0.001)


test_that("padmm output has correct length & matches R output", {
  expect_length(as.numeric(foo), 100)
  expect_equal(as.numeric(foo), bar$beta)
})


