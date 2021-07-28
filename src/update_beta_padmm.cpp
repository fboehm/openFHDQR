#include <Rcpp.h>
#include <Eigen/Dense>
#include "shrink.cpp"
using namespace Rcpp;
using namespace RcppEigen;
using namespace Eigen;



//' Update beta vector for pADMM with weighted L1 penalty
//'
//' @param beta current value of beta vector
//' @param X n by p design matrix
//' @param theta current value of theta vector
//' @param sigma sigma constant value
//' @param eta eta constant value
//' @param y y vector
//' @param z current value of z vector
//' @param lambda L1 penalty parameter
//' @param w weights vector
//' @return updated beta vector for pADMM, as an Eigen::VectorXd object

