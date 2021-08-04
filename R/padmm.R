#' Update beta for the proximal ADMM for weighted L1-penalized quantile regression
#'
#' @param beta current state of the beta parameter vector
#' @param X design matrix
#' @param theta current state of theta vector
#' @param sigma sigma constant, numeric vector of length 1
#' @param eta eta constant, numeric vector of length 1
#' @param y outcome vector
#' @param z current state of z vector
#' @param lambda1 lambda1 penalty parameter
#' @param lambda2 lambda2 penalty parameter
#' @param w weights vector with lambda1
#' @param nu weights vector with lambda2
#' @return updated beta vector
#' @family proximal ADMM for weighted elastic net penalized quantile regression
#' @export

update_beta_padmmR <- function(beta, X, theta, sigma, eta, y, z, lambda1, lambda2 = 0, w, nu){
  new_beta <- beta
  denom <- sigma * eta
  for (i in seq_along(beta)){
    t1 <- beta[i] + X[, i] %*% (theta + sigma * y - sigma * X %*% beta - sigma * z) / denom
    t2 <- lambda * w[i] / denom
    new_beta[i] <- shrink(t1, t2)
  }
  return(new_beta)
}

#' Update z for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
#'
#' @param y y vector
#' @param X design matrix
#' @param beta current state of beta parameter vector (k + 1)
#' @param theta current state of theta parameter vector (k)
#' @param sigma sigma constant
#' @param tau quantile, a number between 0 and 1
#' @return updated z vector
#' @family proximal ADMM for weighted L1 penalized quantile regression
#' @export

update_zR <- function(y, X, beta, theta, sigma, tau){
  new_z <- numeric()
  for (i in seq_along(y)){
    new_z[i] <- prox(xi = y[i] - X[i, ] %*% beta + theta[i] / sigma,
                     alpha = length(y) * sigma,
                     tau = tau)
  }
  return(new_z)
}

#' Update theta for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
#'
#' @param theta current state of theta (k)
#' @param gamma gamma constant
#' @param sigma sigma constant
#' @param X design matrix
#' @param beta current state of beta, (k + 1)
#' @param z current state of z, (k + 1)
#' @param y y vector
#' @return updated theta vector
#' @export

update_thetaR <- function(theta, gamma, sigma, X, beta, z, y){
  return(theta - gamma * sigma * (X %*% beta + z - y))
}

#' Perform proximal ADMM for weighted L1-penalized quantile regression
#'
#' @param beta0 initial value of beta
#' @param z0 initial value of z
#' @param theta0 initial value of theta
#' @param sigma sigma constant, a positive number
#' @param X design matrix
#' @param eta eta constant
#' @param y y vector
#' @param lambda L1 penalty constant
#' @param w weights vector
#' @param tau quantile, a number between 0 and 1
#' @param gamma gamma constant, affects the step length in the theta update step
#' @param maxiter maximum number of iterations
#' @param epsilon1 epsilon1 constant for stopping
#' @param epsilon2 epsilon2 constant for stopping
#' @return beta, the vector of coefficient estimates
#' @export

padmm_L1R <- function(beta0 = rep(0, ncol(X)),
                        z0 = y - X %*% beta0,
                        theta0 = rep(1, nrow(X)),
                        sigma = 0.05,
                        X,
                        eta = eigen(t(X) %*% X)$values[1],
                        y,
                        lambda = 1,
                        w = rep(1, length(beta0)),
                        tau = 0.5,
                        gamma = 0.1,
                        max_iter = 10 ^ 5,
                        epsilon1 = 0.001,
                        epsilon2 = 0.001){
  old_beta <- beta0
  old_z <- z0
  old_theta <- theta0
  beta <- old_beta + 0.001
  z <- old_z
  theta <- old_theta
  iter <- 0
  crit1 <- FALSE; crit2 <- FALSE
  while(iter < max_iter & (!crit1 | !crit2)){
    ## Step 2.1
    new_beta <- update_beta_padmm(beta = beta,
                                  X = X,
                                  theta = theta,
                                  sigma = sigma,
                                  eta = eta,
                                  y = y,
                                  z = z,
                                  lambda = lambda,
                                  w = w)
    old_beta <- beta
    beta <- new_beta
    ## step 2.2
    new_z <- update_z(y = y,
                            X = X,
                            beta = beta,
                            theta = theta,
                            sigma = sigma,
                            tau = tau)
    old_z <- z
    z <- new_z
    ## step 2.3
    new_theta <- update_theta(theta = theta,
                                    gamma = gamma,
                                    sigma = sigma,
                                    X = X,
                                    beta = beta,
                                    z = z,
                                    y = y)
    old_theta <- theta
    theta <- new_theta
    ## check convergence criteria
    crit1 <- check_criterion1(X = X,
                              beta = beta,
                              z = z,
                              y = y,
                              epsilon1 = epsilon1,
                              epsilon2 = epsilon2)
    crit2 <- check_criterion2(sigma = sigma,
                              X = X,
                              z = z,
                              old_z = old_z,
                              epsilon1 = epsilon1,
                              epsilon2 = epsilon2,
                              theta = theta)
    iter <- iter + 1
    print(beta)
  }
  return(list(beta = beta, z = z, theta = theta))
}

#' Check stopping criterion 1
#'
#' @param X design matrix
#' @param beta current value of beta vector
#' @param z current value of z vector
#' @param y y vector
#' @param epsilon1 epsilon1 constant value
#' @param epsilon2 epsilon2 constant value
#' @return a logical vector of length one. TRUE if criterion 1 is satisfied and FALSE otherwise
#' @export

check_criterion1 <- function(X, beta, z, y, epsilon1, epsilon2){
  n <- length(y)
  return(norm_vec(X %*% beta + z - y) <= sqrt(n) * epsilon1 + epsilon2 * max(norm_vec(X %*% beta), norm_vec(z), norm_vec(y)))
}

#' Check stopping criterion 2
#'
#' @param sigma sigma constant
#' @param X design matrix
#' @param z current z vector (k)
#' @param old_z old z vector (k - 1)
#' @param epsilon1 epsilon1 constant
#' @param epsilon2 epsilon2 constant
#' @param theta current theta vector (k)
#' @return a logical vector of length one. TRUE if criterion 2 is satisfied and FALSE otherwise
#' @export

check_criterion2 <- function(sigma, X, z, old_z, epsilon1, epsilon2, theta){
  p <- ncol(X)
  return(sigma * norm_vec(t(X) %*% (z - old_z)) <= sqrt(p) * epsilon1 + epsilon2 * norm_vec(t(X) %*% theta))
}


#' Calculate L2 norm of a vector
#'
#' @param x a vector
#' @return a numeric vector of length one, the l2 norm of x
#' @export
#' @references https://stackoverflow.com/questions/10933945/how-to-calculate-the-euclidean-norm-of-a-vector-in-r
#'

norm_vec <- function(x) sqrt(sum(x^2))
