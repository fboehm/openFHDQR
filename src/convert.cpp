#include <Rcpp.h>
#include <RcppEigen.h>
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
// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
NumericVector toNumericVector(Eigen::VectorXd x) {
  Rcpp::NumericVector xx(wrap(x));
  return xx;
}


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::VectorXd toVectorXd(Rcpp::NumericVector x){
  Eigen::Map<Eigen::VectorXd> XS(Rcpp::as<Eigen::Map<Eigen::VectorXd> >(x));
  return XS;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
