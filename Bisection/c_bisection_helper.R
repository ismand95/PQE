library(Rcpp)
library(RcppArmadillo)

objective <- function(x, m = 15, l = 0.1) {
    return(l - ((x * (x + 1)**m) / ((x + 1)**m + 1)))
}

sourceCpp("c_bisection.cpp")

BiSection(func = objective, x_l = -10, x_r = 10, tol = 1e-5)