#include <cmath>        // std::abs
#include<algorithm>
#include<iostream>
#include <vector>
#include <RcppArmadillo.h>
#include "shrink.h"


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]


//' Update z for the proximal ADMM or scd ADMM for weighted L1-penalized quantile regression
//'
//' @param y y vector
//' @param X design matrix
//' @param beta beta vector
//' @param theta current state of theta parameter vector (k)
//' @param sigma sigma constant
//' @param tau quantile, a number between 0 and 1
//' @return updated z vector
//' @family proximal ADMM for weighted L1 penalized quantile regression
//' @export
// [[Rcpp::export]]
arma::vec update_z(arma::vec y,
                             arma::mat X,
                             arma::vec beta,
                             arma::vec theta,
                         double sigma,
                         double tau){
  const int n = y.n_elem;
  arma::vec new_z;
  new_z.zeros(n);
  arma::vec arg1 = y - X * beta + theta / sigma;
  for(int i = 0; i < n; ++i){
    new_z[i] = prox(arg1[i], sigma * n, tau);
  }
  return new_z;
}


