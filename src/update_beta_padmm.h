#ifndef UPDATE_BETA_PADMM
#define UPDATE_BETA_PADMM


#include <RcppArmadillo.h>

arma::vec update_beta_padmm(arma::vec beta,
                            arma::mat X,
                            arma::vec theta,
                            double sigma,
                            double eta,
                            arma::vec y,
                            arma::vec z,
                            double l1,
                            double l2,
                            arma::vec w,
                            arma::vec nu);

#endif // UPDATE_BETA_PADMM
