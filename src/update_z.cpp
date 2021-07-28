#include <Rcpp.h>
#include <cmath>        // std::abs
#include<algorithm>
#include<iostream>
#include <vector>
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
// [[Rcpp::interfaces(r, cpp)]]
//Rcpp::NumericVector update_z(Rcpp::NumericVector y,
//void update_z(Rcpp::NumericVector y,
//         Rcpp::NumericVector Xbeta,
//                             Rcpp::NumericVector theta,
//                             double sigma,
//                             double tau)
//  {
//  const int n = y.length();
//  Rcpp::NumericVector new_z(n);
//  Rcpp::NumericVector arg1 = y - Xbeta + theta / sigma;
//  for(int i = 0; i < n; ++i){
//    new_z[i] = prox(arg1[i], sigma * n, tau);
//  }

//  return new_z;
//}


