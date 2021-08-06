#ifndef SHRINK
#define SHRINK


#include <RcppArmadillo.h>

double shrink(double u, double alpha);

double prox(double xi, double alpha, double tau);

#endif // SHRINK
