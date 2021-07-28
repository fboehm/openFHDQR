#include <Rcpp.h>
#include <Eigen/Dense>
#include <shrink.cpp>
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

Eigen::VectorXd update_beta_padmm_Rcpp(Eigen::VectorXd beta,
                                           Eigen::MatrixXd X,
                                           Eigen::VectorXd theta,
                                           double sigma,
                                           double eta,
                                           Eigen::VectorXd y,
                                           Eigen::VectorXd z,
                                           double lambda,
                                           Eigen::VectorXd w){
  Eigen::VectorXd new_beta = beta;
  double denom = sigma * eta;
  for (int i = 0; i < X.rows(); i++) {
    double arg1 = beta[i] + X[, i] * (theta + sigma * y - sigma * X * beta - sigma * z) / denom;
    double arg2 =  lambda * w[i] / denom;
    new_beta[i] = shrink(arg1, arg2);
  }
  return new_beta;
}



