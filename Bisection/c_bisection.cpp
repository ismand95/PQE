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
List BiSection(Function func, double x_l, double x_r, double tol)
{
    // initialize variables
    List bisection;
    double x_m; // placeholder mid-value
    double f_m; // placeholder mid-value eval
    int i = 0; // iteration variable

    // validating inputs
    if (x_l >= x_r)
    {
        throw std::invalid_argument("Invalid argument: x_l >= x_r");
    }
    if (CallR(func, x_l) * CallR(func, x_r) > 0)
    {
        throw std::invalid_argument("Invalid argument: f(x_l) * f(x_r) > 0");
    }

    while ((x_r - x_l) > tol)
    {
        x_m = (x_r + x_l) / 2;
        f_m = CallR(func, x_m);

        if (f_m == 0)
        {
            bisection["iterations"] = i;
            bisection["root"] = x_m;
        } else if ((CallR(func, x_l) * CallR(func, x_m)) < 0)
        {
            x_r = x_m;
        } else{
            x_l = x_m;
        }
        i += 1;  
    }
    bisection["iterations"] = i;
    bisection["root"] = (x_r + x_l) / 2;
        
    return bisection;
}