#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace RcppEigen;


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
NumericVector toNumericVector(Eigen::VectorXd x) {
  Rcpp::NumericVector xx(x);
  return xx;
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
NumericMatrix toNumericMatrix(Eigen::MatrixXd X) {
  Rcpp::NumericMatrix XX(X);
  return XX;
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
template <class T, class U>
T toNumericObject (U X) {
  T XX(X);
  return XX;
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
template <class T, class U>
T toEigenObject (U X) {
  T Eigen::Map<U> XX(Eigen::Map<U> X);
  return XX;
}




// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::VectorXd toVectorXd(Rcpp::NumericVector x){
  Eigen::Map<Eigen::VectorXd> XS(Eigen::Map<Eigen::VectorXd> x);
  return XS;
}


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::MatrixXd toMatrixXd(Rcpp::NumericMatrix X){
  Eigen::Map<Eigen::MatrixXd> XS(Eigen::Map<Eigen::MatrixXd> X);
  return XS;
}
