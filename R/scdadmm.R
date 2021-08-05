#' Calculate chi vector
#'
#' @param beta current state of beta vector
#' @param j indicates which component of beta to update
#' @param Xrow ith row of design matrix
#' @param thetai ith entry of current value of theta vector
#' @param sigma sigma constant
#' @param yi ith component of y vector
#' @param zi ith component of current value of z vector
#' @return a 1-vector for use in updating a component of beta

calc_chiiR <- function(beta, j, Xrow, thetai, sigma, yi, zi){
  return(thetai + sigma * (yi - zi - as.numeric(Xrow[- j] %*% beta[- j])))
}

#' Update component of beta with coordinate descent
#'
#' @param beta current value of beta vector
#' @param index an integer from 1 to p, indicating the index of beta for the update
#' @param X design matrix
#' @param theta current value of theta vector
#' @param sigma sigma constant
#' @param y y vector
#' @param z current value of the z vector
#' @param l1 lambda1 L1 penalty parameter
#' @param l2 lambda2 L2 penalty parameter
#' @param w weights vector for lambda1
#' @param nu weights vector for lambda2
#' @return update of one component of beta
#' @export

update_beta_component_scdadmmR <- function(beta, index, X, theta, sigma, y, z, l1, l2, w, nu){
  xj <- X[, index] # jth column of X matrix
  chi <- numeric()
  for (i in seq_along(theta)){
    chi[i] <- calc_chiiR(beta = beta, j = index, Xrow = X[i, ], thetai = theta[i], sigma = sigma, yi = y[i], zi = z[i])
  }
  arg1 <- as.numeric(xj %*% chi)
  arg2 <- l1 * w[index]
  return(shrink(arg1, arg2) / (sigma * norm_vec(xj) ^ 2 + l2 * nu[index]))
}

#' One update of beta with scd
#'
#' @param beta current value of beta vector
#' @param X design matrix
#' @param theta current value of theta vector
#' @param sigma sigma constant
#' @param y y vector
#' @param z current value of the z vector
#' @param l1 lambda1 parameter
#' @param l2 lambda2 parameter
#' @param w weights vector for lambda1
#' @param nu weights vector for lambda2
#' @return update of entirety of beta via scdadmm
#' @export

update_beta_scdadmmR <- function(beta, X, theta, sigma, y, z, l1, l2, w, nu){
  for (j in seq_along(beta)){
    beta[j] <- update_beta_component_scdadmmR(beta = beta,
                                                 index = j,
                                                 X = X,
                                                 theta = theta,
                                                 sigma = sigma,
                                                 y = y,
                                                 z = z,
                                                 l1 = l1,
                                              l2 = l2,
                                                 w = w, nu = nu)
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
#' @param l1 L1 penalty parameter
#' @param l2 L2 penalty parameter
#' @param w L1 weights vector
#' @param nu L2 weights vector
#' @param tau quantile, a number between 0 and 1
#' @param gamma gamma constant, affects the step length in the theta update step
#' @param maxiter maximum number of iterations
#' @param epsilon1 epsilon1 constant for stopping
#' @param epsilon2 epsilon2 constant for stopping
#' @param epsilon3 tolerance for scd updating of beta
#' @return beta, the vector of coefficient estimates
#' @export

scdadmmR <- function(beta0 = rep(1, ncol(X)),
                          z0 = y - X %*% beta0,
                          theta0 = rep(1, nrow(X)),
                        sigma = 0.05,
                        X,
                        eta = eigen(t(X) %*% X)$values[1],
                        y,
                        l1 = 1,
                     l2 = 0,
                        w = rep(1, length(beta0)),
                     nu = rep(1, length(beta0)),
                        tau = 0.5,
                        gamma = 1, # gamma is absent (ie, equal to 1) in scd admm
                        max_iter = 10 ^ 5,
                        epsilon1 = 0.001,
                        epsilon2 = 0.001,
                        epsilon3 = 0.001){
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
    iter2 <- 0
    while(iter2 < max_iter & max(abs(beta - old_beta) > epsilon3)){
      new_beta <- update_beta_scdadmmR(beta = beta,
                          X = X,
                          theta = theta,
                          sigma = sigma,
                          y = y,
                          z = z,
                          l1 = l1,
                          l2 = l2,
                          w = w,
                          nu = nu)
      old_beta <- beta
      beta <- new_beta
      iter2 <- iter2 + 1
    }
    ## step 2.2
    new_z <- update_zR(y = y,
                      X = X,
                      beta = beta,
                      theta = theta,
                      sigma = sigma,
                      tau = tau)
    old_z <- z
    z <- new_z
    ## step 2.3
    new_theta <- update_thetaR(theta = theta,
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


