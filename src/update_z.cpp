#include <Rcpp.h>
#include <cmath>        // std::abs
#include<algorithm>
#include<iostream>

using namespace Rcpp;



//' Update z for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
//'
//' @param y y vector
//' @param Xbeta matrix product of X and beta
//' @param theta current state of theta parameter vector (k)
//' @param sigma sigma constant
//' @param tau quantile, a number between 0 and 1
//' @return updated z vector
//' @family proximal ADMM for weighted L1 penalized quantile regression
//' @export

Eigen::VectorXd update_z(Eigen::VectorXd y,
                         Eigen::MatrixXd Xbeta,
                         Eigen::VectorXd theta,
                         double sigma,
                         double tau){
  Eigen::VectorXd new_z = y;
  for(int i = 0; i < y.size(); i++){
    new_z[i] = prox(y[i] - Xbeta[i] + theta[i] / sigma, sigma * y.size(), tau)
  }
}



update_z <- function(y, X, beta, theta, sigma, tau){
  new_z <- numeric()
  for (i in seq_along(y)){
    new_z[i] <- prox(xi = y[i] - X[i, ] %*% beta + theta[i] / sigma,
                     alpha = length(y) * sigma,
                     tau = tau)
  }
  return(new_z)
}
