#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector poisson_CPP(int iT, double dPhi, double dAlpha) {
  
  NumericVector vY(iT); //vector of observations
  double dL;
  double dY;  

  //initialize at the unconditional value
  dL = dPhi / (1 - dAlpha);
  
  //sample the first observations
  vY[0] = Rf_rpois(dL);
  
  for (int i = 1; i < iT; i++) {
        dL = dPhi + dAlpha * vY[i-1];
        dY = Rf_rpois(dL);
        vY[i] = dY;
  }
  
  return vY;
}





