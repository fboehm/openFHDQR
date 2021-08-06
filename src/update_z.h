#ifndef UPDATE_Z
#define UPDATE_Z


#include <RcppArmadillo.h>

arma::vec update_z(arma::vec y,
                   arma::mat X,
                   arma::vec beta,
                   arma::vec theta,
                   double sigma,
                   double tau);

#endif // UPDATE_Z
