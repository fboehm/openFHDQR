#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
//http://www.kaiyin.co.vu/2014/07/subsetting-matrices-in-armadillo.html
//http://arma.sourceforge.net/docs.html#submat

arma::mat choose_col(arma::mat x, int idx) {
  arma::mat xsub;
  xsub = x.col(idx);
  return xsub;
}
arma::mat choose_row(arma::mat x, int idx) {
  arma::mat xsub;
  xsub = x.row(idx);
  return xsub;
}
arma::mat choose_element(arma::mat x, int ridx, int cidx) {
  arma::mat xsub;
  xsub = x.submat(ridx, cidx, ridx, cidx);
  return xsub;
}
