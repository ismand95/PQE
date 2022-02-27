#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector DynamicPoissonC(int T, double alpha, double phi) {
    double lambda;
    double y_t;
    NumericVector Y_t(T);  // vector of length T
    
    lambda = phi / (1 - alpha);
    Y_t[0] = Rf_rpois(lambda);

    for (int i = 1; i < T; i++)
    {
        lambda = phi + alpha * Y_t[i-1];
        y_t = Rf_rpois(lambda);
        Y_t[i] = y_t;
    }
    return Y_t;
}

