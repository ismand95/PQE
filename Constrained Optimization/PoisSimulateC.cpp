// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// [[Rcpp::export]]
List PoisSimulate_cpp(
    int iT,
    double dAlpha,
    double dPhi)
{

  // Initialize variables and the first element of vLambda and vY
  vec vY(iT);
  vec vLambda(iT);
  List lOut;

  vLambda(0) = dPhi / (1 - dAlpha);
  vY(0) = Rf_rpois(vLambda(0));

  //Computing the remaining elements of vLambda and vY
  for (int t = 1; t < iT; t++)
  {
    vLambda(t) = dPhi + dAlpha * vY(t - 1);
    vY(t) = Rf_rpois(vLambda(t));
  }

  lOut["vLambda"] = vLambda;
  lOut["vY"] = vY;

  return lOut;
}