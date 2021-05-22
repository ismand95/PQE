library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_newton-raphson.cpp")


objective <- function(x) {
    return(cos(x) - x)
}

objective_prime <- function(x) {
    return(-sin(x) - 1)
}

NewtonRaphson(objective, objective_prime, x_0 = 1, tol = 1e-10, maxiter = 10000)