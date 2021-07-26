#include <Rcpp.h>
using namespace Rcpp;
using namespace RcppEigen;


//' Update beta vector for pADMM with weighted L1 penalty
//'
//' @param beta current value of beta vector
//' @param X n by p design matrix
//' @param theta current value of theta vector
//' @param sigma sigma constant value
//' @param eta eta constant value
//' @param y y vector
//' @param z current value of z vector
//' @param lambda L1 penalty parameter
//' @param w weights vector
//' @export
// [[Rcpp::export(rng = false)]]

Rcpp::NumericVector update_beta_padmm_Rcpp(Rcpp::NumericVector beta,
                                           Rcpp::NumericMatrix X,
                                           Rcpp::NumericVector theta,
                                           double sigma,
                                           double eta,
                                           Rcpp::NumericVector y,
                                           Rcpp::NumericVector z,
                                           double lambda,
                                           Rcpp::NumericVector w){

}




update_beta_padmm <- function(beta, X, theta, sigma, eta, y, z, lambda, w){
  new_beta <- beta
  denom <- sigma * eta
  for (i in seq_along(beta)){
    t1 <- beta[i] + X[, i] %*% (theta + sigma * y - sigma * X %*% beta - sigma * z) / denom
    t2 <- lambda * w[i] / denom
    new_beta[i] <- shrink(t1, t2)
  }
  return(new_beta)
}
