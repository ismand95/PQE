// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

double ArithmeticMean(NumericVector x)
{
    // initialize variables
    double mean = 0;

    for (int i = 0; i < x.size(); i++)
    {
        mean += x[i];
    }

    return mean / x.size();
}

// [[Rcpp::export]]
double MonteCarloIntegration(Function func, double n, double a, double b)
{
    // initialize variables
    double I;
    double FunctionMean;

    // draw from uniform distribution (from R)
    NumericVector U = runif(n, a, b);

    FunctionMean = ArithmeticMean(func(U));
    I = (b - a) * FunctionMean;

    return I;
}