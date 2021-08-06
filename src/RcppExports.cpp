// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/openFHDQR.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// padmm
arma::vec padmm(arma::vec beta0, arma::vec z0, arma::vec theta0, double sigma, arma::mat X, double eta, arma::vec y, double l1, double l2, arma::vec w, arma::vec nu, double tau, double gamma, int max_iter, double eps1, double eps2);
RcppExport SEXP _openFHDQR_padmm(SEXP beta0SEXP, SEXP z0SEXP, SEXP theta0SEXP, SEXP sigmaSEXP, SEXP XSEXP, SEXP etaSEXP, SEXP ySEXP, SEXP l1SEXP, SEXP l2SEXP, SEXP wSEXP, SEXP nuSEXP, SEXP tauSEXP, SEXP gammaSEXP, SEXP max_iterSEXP, SEXP eps1SEXP, SEXP eps2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type beta0(beta0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z0(z0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta0(theta0SEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type l1(l1SEXP);
    Rcpp::traits::input_parameter< double >::type l2(l2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type w(wSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< double >::type eps1(eps1SEXP);
    Rcpp::traits::input_parameter< double >::type eps2(eps2SEXP);
    rcpp_result_gen = Rcpp::wrap(padmm(beta0, z0, theta0, sigma, X, eta, y, l1, l2, w, nu, tau, gamma, max_iter, eps1, eps2));
    return rcpp_result_gen;
END_RCPP
}
// shrink
double shrink(double u, double alpha);
static SEXP _openFHDQR_shrink_try(SEXP uSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type u(uSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(shrink(u, alpha));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _openFHDQR_shrink(SEXP uSEXP, SEXP alphaSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_openFHDQR_shrink_try(uSEXP, alphaSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// prox
double prox(double xi, double alpha, double tau);
static SEXP _openFHDQR_prox_try(SEXP xiSEXP, SEXP alphaSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(prox(xi, alpha, tau));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _openFHDQR_prox(SEXP xiSEXP, SEXP alphaSEXP, SEXP tauSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_openFHDQR_prox_try(xiSEXP, alphaSEXP, tauSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// update_beta_padmm
arma::vec update_beta_padmm(arma::vec beta, arma::mat X, arma::vec theta, double sigma, double eta, arma::vec y, arma::vec z, double l1, double l2, arma::vec w, arma::vec nu);
RcppExport SEXP _openFHDQR_update_beta_padmm(SEXP betaSEXP, SEXP XSEXP, SEXP thetaSEXP, SEXP sigmaSEXP, SEXP etaSEXP, SEXP ySEXP, SEXP zSEXP, SEXP l1SEXP, SEXP l2SEXP, SEXP wSEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type l1(l1SEXP);
    Rcpp::traits::input_parameter< double >::type l2(l2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type w(wSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(update_beta_padmm(beta, X, theta, sigma, eta, y, z, l1, l2, w, nu));
    return rcpp_result_gen;
END_RCPP
}
// update_theta
arma::vec update_theta(arma::vec theta, double gamma, double sigma, arma::mat X, arma::vec beta, arma::vec z, arma::vec y);
static SEXP _openFHDQR_update_theta_try(SEXP thetaSEXP, SEXP gammaSEXP, SEXP sigmaSEXP, SEXP XSEXP, SEXP betaSEXP, SEXP zSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(update_theta(theta, gamma, sigma, X, beta, z, y));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _openFHDQR_update_theta(SEXP thetaSEXP, SEXP gammaSEXP, SEXP sigmaSEXP, SEXP XSEXP, SEXP betaSEXP, SEXP zSEXP, SEXP ySEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_openFHDQR_update_theta_try(thetaSEXP, gammaSEXP, sigmaSEXP, XSEXP, betaSEXP, zSEXP, ySEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// update_z
arma::vec update_z(arma::vec y, arma::mat X, arma::vec beta, arma::vec theta, double sigma, double tau);
static SEXP _openFHDQR_update_z_try(SEXP ySEXP, SEXP XSEXP, SEXP betaSEXP, SEXP thetaSEXP, SEXP sigmaSEXP, SEXP tauSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    rcpp_result_gen = Rcpp::wrap(update_z(y, X, beta, theta, sigma, tau));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _openFHDQR_update_z(SEXP ySEXP, SEXP XSEXP, SEXP betaSEXP, SEXP thetaSEXP, SEXP sigmaSEXP, SEXP tauSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_openFHDQR_update_z_try(ySEXP, XSEXP, betaSEXP, thetaSEXP, sigmaSEXP, tauSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _openFHDQR_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("double(*shrink)(double,double)");
        signatures.insert("double(*prox)(double,double,double)");
        signatures.insert("arma::vec(*update_theta)(arma::vec,double,double,arma::mat,arma::vec,arma::vec,arma::vec)");
        signatures.insert("arma::vec(*update_z)(arma::vec,arma::mat,arma::vec,arma::vec,double,double)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _openFHDQR_RcppExport_registerCCallable() { 
    R_RegisterCCallable("openFHDQR", "_openFHDQR_shrink", (DL_FUNC)_openFHDQR_shrink_try);
    R_RegisterCCallable("openFHDQR", "_openFHDQR_prox", (DL_FUNC)_openFHDQR_prox_try);
    R_RegisterCCallable("openFHDQR", "_openFHDQR_update_theta", (DL_FUNC)_openFHDQR_update_theta_try);
    R_RegisterCCallable("openFHDQR", "_openFHDQR_update_z", (DL_FUNC)_openFHDQR_update_z_try);
    R_RegisterCCallable("openFHDQR", "_openFHDQR_RcppExport_validate", (DL_FUNC)_openFHDQR_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_openFHDQR_padmm", (DL_FUNC) &_openFHDQR_padmm, 16},
    {"_openFHDQR_shrink", (DL_FUNC) &_openFHDQR_shrink, 2},
    {"_openFHDQR_prox", (DL_FUNC) &_openFHDQR_prox, 3},
    {"_openFHDQR_update_beta_padmm", (DL_FUNC) &_openFHDQR_update_beta_padmm, 11},
    {"_openFHDQR_update_theta", (DL_FUNC) &_openFHDQR_update_theta, 7},
    {"_openFHDQR_update_z", (DL_FUNC) &_openFHDQR_update_z, 6},
    {"_openFHDQR_RcppExport_registerCCallable", (DL_FUNC) &_openFHDQR_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_openFHDQR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
