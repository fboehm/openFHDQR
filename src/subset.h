#ifndef SUBSET
#define SUBSET

#include <RcppArmadillo.h>

arma::mat choose_col(arma::mat x, int idx);

arma::mat choose_row(arma::mat x, int idx);

arma::mat choose_element(arma::mat x, int ridx, int cidx);

#endif // SUBSET
