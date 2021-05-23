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
    /*
        The bisection takes the interval [x_l; x_r] in which a root lies,
        therefore this algorithm will not fail if no root is found.
    */

    // initialize variables
    List bisection;
    double x_m; // placeholder mid-value
    double f_m; // placeholder mid-value eval
    int i = 0;  // iteration variable

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
            break;
        }
        else if ((CallR(func, x_l) * CallR(func, x_m)) < 0)
        {
            x_r = x_m;
        }
        else
        {
            x_l = x_m;
        }
        i += 1;
    }

    if (f_m == 0)
    {
        bisection["root"] = x_m;
        bisection["value at root"] = CallR(func, x_m);
    }
    else
    {
        bisection["root"] = (x_r + x_l) / 2;
        bisection["value at root"] = CallR(func, (x_r + x_l) / 2);
    }

    bisection["iterations"] = i;
    bisection["convergence"] = "Achieved";

    return bisection;
}