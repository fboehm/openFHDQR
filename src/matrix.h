// Matrix utilities
#ifndef MATRIX_H
#define MATRIX_H

#include <RcppEigen.h>

Rcpp::NumericMatrix matrix_x_matrix(const Rcpp::NumericMatrix& X,
                              const Rcpp::NumericMatrix& Y);

Rcpp::NumericVector matrix_x_vector(const Rcpp::NumericMatrix& X,
                                    const Rcpp::NumericVector& y);


#endif // MATRIX_H

