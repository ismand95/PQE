// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double Fibonacci(double n)
{
    if (n < 2)
    {
        return n;
    }

    return Fibonacci(n - 1) + Fibonacci(n - 2);
}
