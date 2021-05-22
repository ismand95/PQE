library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_secant.cpp")

objective <- function(x) {
    return(cos(x) - x)
}

Secant(func = objective, x_0 = 1, x_1 = 2, tol = 1e-10, maxiter = 1e+4)

# verify solution
uniroot(objective, interval = c(-100, 100))