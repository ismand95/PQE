// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

double CallR(Function func, double x)
{
    // allows passing R function to cpp double
    SEXP func_val = func(x);
    return *REAL(func_val);
}

// [[Rcpp::export]]
List NewtonRaphson(Function func, Function func_prime, double x_0, double tol, int maxiter) {
    // initialize variables
    List newtonraphson;
    int i = 0;

    return newtonraphson;
}