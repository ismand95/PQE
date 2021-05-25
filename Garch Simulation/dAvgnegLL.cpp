// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double dAvgnegLL(vec vY, double dOmega, double dAlpha, double dBeta)
{
  int iT = vY.size();  //Set number of obs
  double dSumLL = 0;   // Initialize likelihood function
  double NegAvgLL = 0; // Initialize negative average likelihood function

  vec vSigma2(iT);                              // Initialize vectors of sigmas
  vSigma2(0) = dOmega / (1.0 - dAlpha - dBeta); //Set first sigma

  for (int t = 1; t < iT; t++)
  {
    vSigma2(t) = dOmega + dAlpha * pow(vY(t - 1), 2.0) + dBeta * vSigma2(t - 1);
  }

  for (int j = 0; j < iT; j++)
  { //Find the sum of log-likelihood
    dSumLL += -log(2 * M_PI) - log(vSigma2(j)) - pow(vY(j), 2) / vSigma2(j);
  }

  NegAvgLL = -1 * (dSumLL / iT);

  return NegAvgLL;
}
