#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace RcppEigen;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
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

// [[Rcpp::export]]

Eigen::VectorXd update_theta(Eigen::VectorXd theta,
                             double gamma,
                             double sigma,
                             Eigen::MatrixXd X,
                             Eigen::VectorXd beta,
                             Eigen::VectorXd z,
                             Eigen::VectorXd y){
  Eigen::VectorXd out = theta - gamma * sigma * (X * beta + z - y);
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
*/
