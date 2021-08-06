#ifndef UPDATE_Z
#define UPDATE_Z


#include <Rcpp.h>

arma::vec update_z(arma::vec y,
                   arma::vec Xbeta,
                   arma::vec theta,
                   double sigma,
                   double tau);

#endif // UPDATE_Z
