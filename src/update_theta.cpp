#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;



//' Update theta for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
//'
//' @param gamma gamma constant
//' @param sigma sigma constant
//' @param X design matrix
//' @param beta current state of beta, (k + 1)
//' @param z current state of z, (k + 1)
//' @param y y vector
//' @return updated theta vector
//' @export
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export()]]
arma::vec update_theta_diff(
                             double gamma,
                             double sigma,
                             arma::mat X,
                             arma::vec beta,
                             arma::vec z,
                             arma::vec y){
  arma::vec out = - gamma * sigma * (X * beta + z - y);
  return out;
}


