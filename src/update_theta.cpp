#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;



//' Update theta for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
//'
//' @param theta current state of theta (k)
//' @param gamma gamma constant
//' @param sigma sigma constant
//' @param X design matrix
//' @param beta current state of beta, (k + 1)
//' @param z current state of z, (k + 1)
//' @param y y vector
//' @return updated theta vector
//' @export
// [[Rcpp::interfaces(r, cpp)]]
arma::vec update_theta(arma::vec theta,
                             double gamma,
                             double sigma,
                             arma::mat X,
                             arma::vec beta,
                             arma::vec z,
                             arma::vec y){
  arma::vec out = theta - gamma * sigma * (X * beta + z - y);
  return out;
}


