library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_goldensection.cpp")

objective <- function(x) {
    if (x == 0) {
        return(0.0)
    } else {
        return(abs(x) * log(abs(x) / 2) * exp(-abs(x)))
    }
}

GoldenSection(func = objective, x_l = 0.5, x_m = 5.0, x_r = 10.0, tol = 1e-10)

# verify
optim(0.5, objective, method = "BFGS", control = list(fnscale = -1))
