#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector DynamicPoisson(int T, double phi = 0.5, double alpha = 0.7)
{
    // initialize variables
    NumericVector poi(T);
    double lambda;

    // setting initials
    lambda = phi / (1 - alpha);
    poi(0) = Rf_rpois(lambda);

    for (int t = 1; t < T; t++)
    {
        lambda = phi + alpha * poi(t - 1);
        poi(t) = Rf_rpois(lambda);
    }
    

    return poi;
}