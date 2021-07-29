#include <Rcpp.h>
#include <cmath>        // std::abs
#include<algorithm>
#include<iostream>
#include <vector>
#include <RcppEigen.h>
#include "shrink.h"


// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace RcppEigen;

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
// [[Rcpp::interfaces(r, cpp)]]
Eigen::VectorXd update_z(Eigen::VectorXd y,
                         Eigen::VectorXd Xbeta,
                         Eigen::VectorXd theta,
                         double sigma,
                         double tau){
  const int n = y.size();
  Eigen::VectorXd new_z(n);
  Eigen::VectorXd arg1 = y - Xbeta + theta / sigma;
  for(int i = 0; i < n; ++i){
    new_z[i] = prox(arg1[i], sigma * n, tau);
  }
  return new_z;
}


