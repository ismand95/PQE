// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

double CallR(Function func, double x)
{
  // allows passing R function to cpp double
  SEXP func_val = func(x);
  return *REAL(func_val);
}

//[[Rcpp::export]]
List Newton(
    Function f,
    Function f_p,
    Function f_pp,
    double dX0,
    double dTol = 1e-9,
    double maxiter = 2000)
{
  // initialize variables
  double dX = dX0;
  int i = 0;
  List lOut;

  while ((abs(CallR(f_p, dX)) > dTol) && (i < maxiter))
  {
    dX = dX - CallR(f_p, dX) / CallR(f_pp, dX);
    i += 1;
  }

  if (abs(CallR(f_p, dX)) > dTol)
  {
    lOut["optimum"] = NA_REAL;
    lOut["iterations"] = maxiter;
    lOut["f.optimum"] = NA_REAL;
    lOut["f_prime.optimum"] = NA_REAL;
    lOut["f_sec.optimum"] = NA_REAL;
    lOut["type"] = NULL;
    lOut["convergence"] = "Not achieved";
    return lOut;
  }
  else
  {
    lOut["optimum"] = dX;
    lOut["iterations"] = i;
    lOut["f.optimum"] = CallR(f, dX);
    lOut["f_prime.optimum"] = CallR(f_p, dX);
    lOut["f_sec.optimum"] = CallR(f_pp, dX);
    lOut["convergence"] = "Achieved";

    if (CallR(f_pp, dX) < 0.0)
    {
      lOut["type"] = "Local maximum";
    }
    else
    {
      lOut["type"] = "Local minimum";
    }

    return lOut;
  }
}