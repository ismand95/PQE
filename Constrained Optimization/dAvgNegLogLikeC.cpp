// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// [[Rcpp::export]]
double dAvgNegLogLike(
    vec vY,
    double dPhi,
    double dAlpha)
{

  // Initialize variables and the first element of vLambda
  int iT = vY.size();
  double dSumLL = 0;
  double NegAvgLL = 0;
  vec vLambda(iT);

  vLambda(0) = dPhi / (1 - dAlpha);

  //Computing the remaining elements of vLambda
  for (int t = 1; t < iT; t++)
  {
    vLambda(t) = dPhi + dAlpha * vY(t - 1);
  }

  //Find the sum of log-likelihood and finally define the negative average LL
  for (int j = 0; j < iT; j++)
  {
    dSumLL += Rf_dpois(vY(j), vLambda(j), 1);
  }

  NegAvgLL = -1 * (dSumLL / iT);

  return NegAvgLL;
}
