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
///////////// Newton's method //////////////

//[[Rcpp::export]]
List NR_optim_CPP(Function f, Function f_p, Function f_pp, double dX0, double dTol = 1e-5, double maxiter = 2000) {
  
  double dX = dX0;
  int iter = 0;
  
  while((abs(CallR(f, dX))) > dTol && (iter < maxiter)) {
    dX = dX - CallR(f_p, dX) / CallR(f_pp, dX);
    iter = iter + 1;
  }
  
  if (abs(CallR(f_p, dX)) > dTol) {
    List lOut;
    lOut["Convergence"] = "Algorithm failed to converge"; 
    return lOut;
  } else {
    List lOut;
    lOut["X value"] = dX;
    lOut["Function value"] = CallR(f, dX);
    lOut["Iterations"] = iter;
    lOut["Convergence"] = "Algorithm converged"; 
    return lOut;
  }
}