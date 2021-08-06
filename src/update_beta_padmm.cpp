#include <RcppArmadillo.h>
#include "shrink.h"
#include "subset.h"
//[[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//
//' Update beta for the proximal ADMM for weighted elastic net-penalized quantile regression
//'
//' @param beta current state of the beta parameter vector
//' @param X design matrix
//' @param theta current state of theta vector
//' @param sigma sigma constant, numeric vector of length 1
//' @param eta eta constant, numeric vector of length 1
//' @param y outcome vector
//' @param z current state of z vector
//' @param l1 lambda1 penalty parameter
//' @param l2 lambda2 penalty parameter
//' @param w weights vector for L1 penalty
//' @param nu weights vector for L2 penalty
//' @return updated beta vector
//' @family proximal ADMM for weighted L1 penalized quantile regression
//' @export
// [[Rcpp::export]]
arma::vec update_beta_padmm(arma::vec beta,
                                      arma::mat X,
                                      arma::vec theta,
                                      double sigma,
                                      double eta,
                                      arma::vec y,
                                      arma::vec z,
                                      double l1,
                                      double l2,
                                      arma::vec w,
                                      arma::vec nu){
  const int p = beta.size();
  arma::vec new_beta(p);
  arma::vec denom = sigma * eta + l2 * nu;
  arma::vec arg1 = (theta + sigma * y - sigma * X * beta - sigma * z) / denom;
  for (int j = 0; j < p; ++j){
    arma::mat Xcol = choose_col(X, j);
    double t1 = sigma * eta * beta[j] + (Xcol * arg1).eval()(0,0);
    double t2 = l1 * w[j];
    new_beta[j] = shrink(t1, t2) / denom[j];
  }
  return new_beta;
}



