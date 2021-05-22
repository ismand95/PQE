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
List GoldenSection(Function func, double x_l, double x_m, double x_r, double tol)
{
    // initialize variables
    List golden;
    double y;
    double rho = (1 + pow(5, 0.5)) / 2; // Golden Ratio
    int i = 0;                          // iteration variable

    if (x_l >= x_m)
    {
        throw std::invalid_argument("Invalid argument: x_l >= x_m");
    }
    if (x_m >= x_r)
    {
        throw std::invalid_argument("Invalid argument: x_m >= x_r");
    }
    if (CallR(func, x_l) > CallR(func, x_m))
    {
        throw std::invalid_argument("Invalid argument: func(x_l) > func(x_m)");
    }
    if (CallR(func, x_r) > CallR(func, x_m))
    {
        throw std::invalid_argument("Invalid argument: func(x_r) > func(x_m)");
    }

    while ((x_r - x_l) > tol)
    {
        if ((x_r - x_m) > (x_m - x_l))
        {
            y = x_m + (x_r - x_m) / (1 + rho);

            if (CallR(func, y) >= CallR(func, x_m))
            {
                x_l = x_m;
                x_m = y;
            }
            else
            {
                x_r = y;
            }
        }
        else
        {
            y = x_m - (x_m - x_l) / (1 + rho);

            if (CallR(func, y) >= CallR(func, x_m))
            {
                x_r = x_m;
                x_m = y;
            }
            else
            {
                x_l = y;
            }
        }
        i += 1;
        Rcout << i << ": " << x_m << "\n";
    }

    golden["optimum"] = x_m;
    golden["optimum evaluated"] = CallR(func, x_m);
    golden["iterations"] = i;
    return golden;
}