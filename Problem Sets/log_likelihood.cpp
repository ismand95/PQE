#include <Rcpp.h>
using namespace Rcpp;

double SumVector(NumericVector x) {
    double total = 0;

    for (int i = 0; i < x.size(); i++)
    {
        total += x[i];
    }
    return total;
}

// [[Rcpp::export]]
double DynamicPoissonLLC(int T, double alpha, double phi) {
    double lambda;
    double y_t;
    NumericVector LL_t(T);
    NumericVector Y_t(T);  // vector of length T
    
    lambda = phi / (1 - alpha);
    y_t = Rf_rpois(lambda);

    Y_t[0] = y_t;
    LL_t[0] = Rf_dpois(y_t, lambda, 1);

    for (int i = 1; i < T; i++)
    {
        lambda = phi + alpha * Y_t[i-1];
        y_t = Rf_rpois(lambda);
        
        Y_t[i] = y_t;
        LL_t[i] = Rf_dpois(y_t, lambda, 1);
    }

    double NegAvgLL;
    NegAvgLL = (SumVector(LL_t) / LL_t.size()) * (-1);

    return NegAvgLL;
}