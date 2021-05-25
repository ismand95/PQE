# R helper Newton multivariate optimization

library(Rcpp)
library(RcppArmadillo)
sourceCpp("c_newton.cpp")

# setting up function for testing the algorithm
grad <- function(vX) {
    dA <- vX[1]^2 / 2 - vX[2]^2 / 4
    dB <- 2 * vX[1] - exp(vX[2])

    f1 <- cos(dA) * cos(dB) * vX[1] - sin(dA) * sin(dB) * 2
    f2 <- -cos(dA) * cos(dB) * vX[2] / 2 + sin(dA) * sin(dB) * exp(vX[2])
    dOut <- c(f1, f2)
    return(dOut)
}

hessian <- function(vX) {
    dA <- vX[1]^2 / 2 - vX[2]^2 / 4
    dB <- 2 * vX[1] - exp(vX[2])

    f11 <- -sin(dA) * cos(dB) * (4 + vX[1]^2) + cos(dA) * cos(dB) - cos(dA) * sin(dB) * 4 * vX[1]
    f12 <- sin(dA) * cos(dB) * (vX[1] * vX[2] / 2 + 2 * exp(vX[2])) + cos(dA) * sin(dB) * (vX[1] * exp(vX[2]) + vX[2])
    f22 <- -sin(dA) * cos(dB) * (vX[2]^2 / 4 + exp(2 * vX[2])) - cos(dA) * cos(dB) / 2 - cos(dA) * sin(dB) * vX[2] * exp(vX[2]) + sin(dA) * sin(dB) * exp(vX[2])

    dOut <- matrix(data = c(f11, f12, f12, f22), nrow = 2, ncol = 2, byrow = 1)
}

vX0 <- c(1.6, 1.2) # starting values
fNewtonMulti(grad, hessian, vX0, dEps = 1e-9, iMax = 1000) # testing function
