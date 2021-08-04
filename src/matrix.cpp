// [[Rcpp::depends(RcppEigen)]]
#include "convert.h"
#include "matrix.h"
#include <RcppEigen.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace Eigen;


// matrix multiplication
// From https://github.com/rqtl/qtl2/blob/master/src/matrix.cpp
NumericMatrix matrix_x_matrix(const NumericMatrix& X,
                              const NumericMatrix& Y)
{
  const MatrixXd XX(toMatrixXd(X));
  const MatrixXd YY(toMatrixXd(Y));

  if(XX.cols() != YY.rows())
    throw std::range_error("ncol(X) != nrow(Y)");

  NumericMatrix result(XX * YY);
  return(result);
}

// matrix multiplication
// From https://github.com/rqtl/qtl2/blob/master/src/matrix.cpp
NumericVector matrix_x_vector(const NumericMatrix& X,
                              const NumericVector& y)
{
  const MatrixXd XX(toMatrixXd(X));
  const VectorXd yy(toVectorXd(y));

  if(XX.cols() != yy.size())
    throw std::range_error("ncol(X) != length(y)");

  NumericVector result(XX * yy);
  return(result);
}



