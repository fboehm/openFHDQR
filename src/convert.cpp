#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace RcppEigen;


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
