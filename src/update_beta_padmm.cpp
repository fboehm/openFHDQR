#include <Rcpp.h>
#include "shrink.h"
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
//' Update beta for the proximal ADMM for weighted L1-penalized quantile regression
//'
//' @param beta current state of the beta parameter vector
//' @param X design matrix
//' @param theta current state of theta vector
//' @param sigma sigma constant, numeric vector of length 1
//' @param eta eta constant, numeric vector of length 1
//' @param y outcome vector
//' @param z current state of z vector
//' @param lambda lambda penalty parameter
//' @param w weights vector
//' @return updated beta vector
//' @family proximal ADMM for weighted L1 penalized quantile regression
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector update_beta_padmm(Rcpp::NumericVector beta,
                                      Rcpp::NumericMatrix X,
                                      Rcpp::NumericVector theta,
                                      double sigma,
                                      double eta,
                                      Rcpp::NumericVector y,
                                      Rcpp::NumericVector z,
                                      double lambda,
                                      Rcpp::NumericVector w){
  int p = beta.size();
  Rcpp::NumericVector new_beta(p);
  double denom = sigma * eta;
  Rcpp::NumericVector arg2 = (theta + sigma * y - sigma * X * beta - sigma * z) / denom;
  double ld = lambda / denom;
  Rcpp::NumericVector t1vec = beta + X * arg2; //check this! Need transpose of X!
  for (int i = 0; i < p; ++i){
    double t1 = t1vec[i];
    double t2 = ld * w[i];
    new_beta[i] <- shrink(t1, t2);
  }
  return new_beta;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
*/
