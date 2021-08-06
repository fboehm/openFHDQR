#ifndef CHECK_CRITERIA
#define CHECK_CRITERIA


#include <RcppArmadillo.h>

bool check_criterion1(arma::mat X,
                      arma::vec beta,
                      arma::vec z,
                      arma::vec y,
                      double eps1,
                      double eps2);
bool check_criterion2(double sigma,
                      arma::mat X,
                      arma::vec z,
                      arma::vec old_z,
                      double eps1,
                      double eps2,
                      arma::vec theta);
double l2_norm(arma::vec const& u);


#endif // CHECK_CRITERIA
