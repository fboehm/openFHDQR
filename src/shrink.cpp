#include <RcppArmadillo.h>
#include <cmath>        // std::abs
#include<algorithm>
#include<iostream>
#include "shrink.h"

using namespace Rcpp;



// sign function

template <typename T> int sgn(T val) {
  return (T(0) < val) - (val < T(0));
}



// [[Rcpp::interfaces(r, cpp)]]




//' Soft shrinkage operator per Gu et al. 2018
//'
//' @param u a numeric vector of length one
//' @param alpha a numeric vector of length one
//' @return a numeric vector of length one, the output of the soft shrinkage operator
// [[Rcpp::export()]]

double shrink(double u, double alpha){
  int s = sgn(u);
  double a = std::abs(u);
  return s * std::max(a - alpha, 0.0);
}


//' Perform proximal mapping of rho tau
//'
//' @param xi a single number
//' @param alpha a number
//' @param tau a number between 0 and 1, the quantile of interest
//' @return output of the proximal mapping of rho tau
// [[Rcpp::export()]]

double prox(double xi, double alpha, double tau){
  double arg2 = std::min(xi, tau / alpha);
  double arg1 = (tau - 1) / alpha;
  return xi - std::max(arg1, arg2);
}




