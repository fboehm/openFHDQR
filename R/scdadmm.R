#' Update component of beta with coordinate descent
#'
#' @param beta current value of beta vector, ie, a scalar
#' @param index an integer from 1 to p, indicating the index of beta for the update
#' @param X design matrix
#' @param theta current value of theta vector
#' @param sigma sigma constant
#' @param y y vector
#' @param z current value of the z vector
#' @param lambda lambda constant
#' @param w w vector
#' @return update of one component of beta
#' @export

update_beta_component_scdadmm <- function(beta, index, X, theta, sigma, y, z, lambda, w){
  xj <- X[, index] # jth column of X matrix
  arg1a <- as.numeric(X[ , - index] %*% beta[- index])
  arg1b <- theta + sigma * (y - z - arg1a)
  arg1 <- as.numeric(xj %*% arg1b)
  arg2 <- lambda * w[index]
  return(shrink(arg1, arg2) / (sigma * as.numeric((xj %*% xj))))
}

#' One update of beta with scd
#'
#' @param beta current value of beta vector
#' @param X design matrix
#' @param theta current value of theta vector
#' @param sigma sigma constant
#' @param y y vector
#' @param z current value of the z vector
#' @param lambda lambda constant
#' @param w w vector
#' @return update of entirety of beta via scdadmm
#' @export

update_beta_scdadmm <- function(beta, X, theta, sigma, y, z, lambda, w){
  for (index in seq_along(beta)){
    beta[index] <- update_beta_component_scdadmm(beta = beta,
                                                 index = index,
                                                 X = X,
                                                 theta = theta,
                                                 sigma = sigma,
                                                 y = y,
                                                 z = z,
                                                 lambda = lambda,
                                                 w = w)
  }
  return(beta)
}

#' Perform scd ADMM for weighted L1-penalized quantile regression
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
#' @param epsilon3 tolerance for scd updating of beta
#' @return beta, the vector of coefficient estimates
#' @export

qr_scdadmm_L1 <- function(beta0 = rep(0, ncol(X)),
                          z0 = rep(1, nrow(X)),
                          theta0 = rep(1, nrow(X)),
                        sigma = 0.05,
                        X,
                        eta = 1000,
                        y,
                        lambda = 1,
                        w = rep(1, length(beta0)),
                        tau = 0.5,
                        gamma = 1, # gamma is absent (ie, equal to 1) in scd admm
                        max_iter = 10 ^ 5,
                        epsilon1 = 0.001,
                        epsilon2 = 0.001,
                        epsilon3 = 1 / 10 ^ 6){
  old_beta <- beta0
  old_z <- z0
  old_theta <- theta0
  beta <- old_beta + 10 * epsilon3 # Consider other starting point!
  z <- old_z
  theta <- old_theta
  iter <- 0
  crit1 <- FALSE; crit2 <- FALSE
  while(iter < max_iter & (!crit1 | !crit2)){
    ## Step 2.1
    while(max(abs(beta - old_beta) > epsilon3)){
      new_beta <- update_beta_scdadmm(beta = beta,
                          X = X,
                          theta = theta,
                          sigma = sigma,
                          y = y,
                          z = z,
                          lambda = lambda,
                          w = w)
      old_beta <- beta
      beta <- new_beta
    }
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
  }
  return(list(beta = beta, z = z, theta = theta))
}


