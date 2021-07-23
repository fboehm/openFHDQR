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
  xj <- X[ , index]
  arg1 <- xj %*% (theta + sigma * (y - z - xj[- index] %*% beta[- index]))
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

qr_scdadmm_L1 <- function(beta0,
                        z0,
                        theta0,
                        sigma = 1,
                        X,
                        eta = 1,
                        y,
                        lambda,
                        w,
                        tau,
                        gamma = 0.1,
                        maxiter = 10 ^ 5,
                        epsilon1 = 0.001,
                        epsilon2 = 0.001,
                        epsilon3 = 0.001){
  old_beta <- beta0
  old_z <- z0
  old_theta <- theta0
  beta <- old_beta
  z <- old_z
  theta <- old_theta
  iter <- 0
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
  return(beta)
}


