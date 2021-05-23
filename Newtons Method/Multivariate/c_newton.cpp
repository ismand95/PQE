// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//
// The algorithm finds the optimum of a function on an interval and returns a list
//
// f is the objective function
// fGrad is the analytical gradient
// fHessian is the analytical hessian
// vX0 is the vector containing the initial value
// dEps is the prespecified tolerance level
// iMax is the maximal number of interations
//
// The algorithm is not guaranteed to converge
//

// [[Rcpp::export]]
List fNewtonMulti(Function fGrad, Function fHessian, colvec vX0, double dEps = 1e-9, double iMax = 1000)
{
  SEXP fGrad0 = fGrad(vX0);
  SEXP fHessian0 = fHessian(vX0);
  colvec vfGrad0 = as<colvec>(fGrad0);
  mat vfHessian0 = as<mat>(fHessian0);

  double iT = 0;
  double absX = max(abs(vfGrad0));

  while (((iT < iMax) && (absX > dEps)))
  {
    SEXP fGrad0 = fGrad(vX0);
    SEXP fHessian0 = fHessian(vX0);
    colvec vfGrad0 = as<colvec>(fGrad0);
    mat vfHessian0 = as<mat>(fHessian0);

    colvec vX1 = vX0 - inv(vfHessian0) * vfGrad0;

    vX0 = vX1;
    fGrad0 = fGrad(vX0);
    fHessian0 = fHessian(vX0);
    vfGrad0 = as<colvec>(fGrad0);
    vfHessian0 = as<colvec>(fHessian0);

    iT = iT + 1;
    absX = max(abs(vfGrad0));
    Rcout << "The value of vX and vfGrad is " << vX0.t() << " and " << vfGrad0.t() << " at iteration " << iT << "\n";
  }

  if (absX > dEps)
  {
    stop("Algorithm failed to converge (1)\n");
  }
  else if (iT == iMax)
  {
    stop("Algorithm failed to converge (2)\n");
  }
  else
  {
    String sCon = "True";

    fGrad0 = fGrad(vX0);
    vfGrad0 = as<colvec>(fGrad0);

    List lOut;
    lOut["xVal"] = vX0;
    lOut["fprimeVal"] = vfGrad0;
    lOut["Iterations"] = iT;
    lOut["Converged"] = sCon;
    lOut["AbsX"] = absX;

    return (lOut);
  }
}
