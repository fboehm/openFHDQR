#ifndef CONVERT
#define CONVERT


#include <Rcpp.h>
Rcpp::NumericVector toNumericVector(Eigen::VectorXd x);
Eigen::VectorXd toVectorXd(Rcpp::NumericVector x);

#endif
