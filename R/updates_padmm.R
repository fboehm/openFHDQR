#' Update beta for the proximal ADMM for weighted L1-penalized quantile regression
#'
#' @param beta current state of the beta parameter vector
#' @param X design matrix
#' @param theta current state of theta vector
#' @param sigma sigma constant, numeric vector of length 1
#' @param eta eta constant, numeric vector of length 1
#' @param y outcome vector
#' @param z current state of z vector
#' @param lambda lambda penalty parameter
#' @param w weights vector
#' @return updated beta vector
#' @family proximal ADMM for weighted L1 penalized quantile regression

update_beta_padmm <- function(beta, X, theta, sigma, eta, y, z, lambda, w){
  new_beta <- beta
  denom <- sigma * eta
  for (i in seq_along(beta)){
    t1 <- (beta[i] + X[i, ] %*% (theta + sigma * y - sigma * X %*% beta - sigma * z)) / denom
    t2 <- lambda * w[i] / denom
    new_beta[i] <- shrink(t1, t2)
  }
  return(new_beta)
}

#' Update z for the proximal ADMM for weighted L1-penalized quantile regression
#'
#' @param y y vector
#' @param
#' @family proximal ADMM for weighted L1 penalized quantile regression


