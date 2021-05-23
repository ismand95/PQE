# R helper Newton_univariate

library(Rcpp)
sourceCpp("Newton_univariate.cpp")

# function for testing
test_fn <- function(dX) {
   dOut <- cos(dX) - dX
   return(dOut)
}

test_fn_p <- function(dX) {
   dOut <- -sin(dX) - 1
   return(dOut)
}

test_fn_pp <- function(dX) {
   dOut <- -cos(dX)
   return(dOut)
}

plot(seq(-5, 7, 0.001), test_fn(seq(-5, 7, 0.001)))


# Testing function
NR_optim_CPP(test_fn, test_fn_p, test_fn_pp, 1, 1e-5, 2000)
