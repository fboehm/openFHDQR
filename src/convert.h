#ifndef CONVERT
#define CONVERT


#include <Rcpp.h>
Rcpp::NumericVector toNumericVector(Eigen::VectorXd x);
Eigen::VectorXd toVectorXd(Rcpp::NumericVector x);
Rcpp::NumericMatrix toNumericMatrix(Eigen::MatrixXd X);
Eigen::MatrixXd toMatrixXd(Rcpp::NumericMatrix X);


#endif
