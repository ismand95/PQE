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

    while ((i < maxiter) && (abs(CallR(func, x_0)) > tol))
    {
        x_0 = x_0 - (CallR(func, x_0) / CallR(func_prime, x_0));
        i += 1;

        // comment out to not print
        Rcout << i << ": " << x_0 << "\n";
    }
    
    if (abs(CallR(func, x_0)) < tol)
    {
        newtonraphson["root"] = x_0;
        newtonraphson["iterations"] = i;
        newtonraphson["value at root"] = CallR(func, x_0);
        newtonraphson["convergence"] = "Achieved";

        return newtonraphson;
    } else
    {
        newtonraphson["root"] = NA_REAL;
        newtonraphson["iterations"] = i;
        newtonraphson["value at root"] = NA_REAL;
        newtonraphson["convergence"] = "Not achieved";

        return newtonraphson;
    }
}