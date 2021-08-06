#include <RcppArmadillo.h>
#include <math.h> //sqrt
using namespace Rcpp;




bool check_criterion1(arma::mat X,
                      arma::vec beta,
                      arma::vec z,
                      arma::vec y,
                      double eps1,
                      double eps2){
  const int n = y.n_elem;
  bool result = l2_norm(X * beta + z - y) <= sqrt(n) * eps1 + eps2 * max(l2_norm(X * beta), l2_norm(z), l2_norm(y));
  return result;
}

bool check_criterion2(double sigma,
                      arma::mat X,
                      arma::vec z,
                      arma::vec old_z,
                      double eps1,
                      double eps2,
                      arma::vec theta){
  int p = X.n_cols;
  bool result = sigma * l2_norm(X.t() * (z - old_z)) <= sqrt(p) * eps1 + eps2 * l2_norm(X.t() * theta);
  return result;
}







// http://polaris.s.kanazawa-u.ac.jp/~npozar/basic-cpp-for-numerics-vectors.html

double l2_norm(arma::vec const& u){
  double accum = 0.0;
  for (int i = 0; i < u.n_elem; ++i) {
    accum += u[i] * u[i];
  }
  return sqrt(accum);
}


