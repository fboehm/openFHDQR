#ifndef UPDATE_THETA
#define UPDATE_THETA


#include <RcppArmadillo.h>

arma::vec update_theta(arma::vec theta,
                       double gamma,
                       double sigma,
                       arma::mat X,
                       arma::vec beta,
                       arma::vec z,
                       arma::vec y);


#endif // UPDATE_THETA
