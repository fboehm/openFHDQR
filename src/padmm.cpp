#include "update_beta_padmm.h"
#include "update_theta.h"
#include "update_z.h"
#include "check_criteria.h"

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;



//' Perform proximal ADMM for weighted L1-penalized quantile regression
//'
//' @param beta0 initial value of beta
//' @param z0 initial value of z
//' @param theta0 initial value of theta
//' @param sigma sigma constant, a positive number
//' @param X design matrix
//' @param eta eta constant
//' @param y y vector
//' @param l1 L1 penalty parameter
//' @param l2 L2 penalty parameter
//' @param w weights vector for lambda1 penalty
//' @param nu weights vector for lambda2 penalty
//' @param tau quantile, a number between 0 and 1
//' @param gamma gamma constant, affects the step length in the theta update step
//' @param maxiter maximum number of iterations
//' @param eps1 epsilon1 constant for stopping
//' @param eps2 epsilon2 constant for stopping
//' @return beta, the vector of coefficient estimates
//' @export
// [[Rcpp::export()]]
arma::vec padmm(arma::vec beta0,
                arma::vec z0,
                arma::vec theta0,
                double sigma,
                arma::mat X,
                double eta,
                arma::vec y,
                double l1,
                double l2,
                arma::vec w,
                arma::vec nu,
                double tau,
                double gamma,
                int max_iter,
                double eps1,
                double eps2){
  arma::vec old_beta = beta0;
  arma::vec old_z = z0;
  arma::vec old_theta = theta0;
  arma::vec beta = 2 * old_beta;
  arma::vec z = old_z;
  arma::vec theta = old_theta;
  int iter = 0;
  bool crit1 = false;
  bool crit2 = false;
  while (iter < max_iter && (!crit1 || !crit2)) {
    // Step 2.1
    arma::vec new_beta = update_beta_padmm(beta = beta,
                                   X = X,
                                   theta = theta,
                                   sigma = sigma,
                                   eta = eta,
                                   y = y,
                                   z = z,
                                   l1 = l1,
                                   l2 = l2,
                                   w = w,
                                   nu = nu);
    old_beta = beta;
    beta = new_beta;
    // Step 2.2
    arma::vec new_z = update_z(y = y,
                       X = X,
                       beta = beta,
                       theta = theta,
                       sigma = sigma,
                       tau = tau);
    old_z = z;
    z = new_z;
    // Step 2.3
    arma::vec new_theta = update_theta(theta = theta,
                               gamma = gamma,
                               sigma = sigma,
                               X = X,
                               beta = beta,
                               z = z,
                               y = y);
    old_theta = theta;
    theta = new_theta;
    // check convergence criteria
    bool crit1 = check_criterion1(X = X,
                              beta = beta,
                              z = z,
                              y = y,
                              eps1 = eps1,
                              eps2 = eps2);
    bool crit2 = check_criterion2(sigma = sigma,
                                X = X,
                                z = z,
                                old_z = old_z,
                                eps1 = eps1,
                                eps2 = eps2,
                                theta = theta);
      iter = iter + 1;
  }
  return beta;
}

