// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;

int Indicator(double x) {
    if (x < 0.0)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

double Sqrt(double x) {
    return pow(x, 0.5);
}

// [[Rcpp::export]]
List GarchSimulation(int iT, double dOmega, double dAlpha, double dGamma, double dBeta) {
    // variables
    vec vY(iT);
    vec vS(iT);

    // parameter constraints
    if (dOmega < 0 || dAlpha < 0 || dGamma < 0 || dBeta < 0)
    {
        throw std::invalid_argument("Positiveness not achieved");
    }

    if ((dAlpha + (dGamma / 2) + dBeta) > 1.0)
    {
        throw std::invalid_argument("Covariance stationary");
    }

    // initial values
    vS(0) = dOmega / (1.0 - dAlpha - (dGamma / 2.0) - dBeta);
    vY(0) = Sqrt(vS(0)) * Rf_rnorm(0.0, 1.0);

    // main logic
    for (int i = 1; i < iT; i++)
    {
        vS(i) = (dOmega 
                + (dAlpha + dGamma * Indicator(vY(i - 1))) * pow(vY(i - 1), 2.0) 
                + dBeta * vS(i - 1));
        vY(i) = Sqrt(vS(i)) * Rf_rnorm(0.0, 1.0);
    }

    List lSigma;
    lSigma["vS"] = vS;
    lSigma["vY"] = vY;

    return lSigma;
}
