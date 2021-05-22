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
List Secant(Function func, double x_0, double x_1, double tol, double maxiter)
{
    // initialize variables
    double x_2;
    List secant;

    for (int i = 0; i < maxiter; i++)
    {
        x_2 = x_1 - CallR(func, x_1) * ((x_0 - x_1) / (CallR(func, x_0) - CallR(func, x_1)));

        if (abs(CallR(func, x_2)) < tol)
        {
            secant["root"] = x_2;
            secant["iterations"] = i;
            secant["value at root"] = CallR(func, x_2);
            secant["convergence"] = "Achieved";

            return secant;
        }

        x_0 = x_1;
        x_1 = x_2;
    }
    secant["root"] = NA_REAL;
    secant["iterations"] = maxiter;
    secant["value at root"] = NA_REAL;
    secant["convergence"] = "Not achieved";

    return secant;
}