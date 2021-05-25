# R helper Newton_univariate
library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_newton.cpp")

# function for testing
objective <- function(x) {
   return(
      x^3 + (6 - x)^2
   )
}

objective_prime <- function(x) {
   return(
      3 * x^2 + 2 * x - 12
   )
}

objective_sec <- function(x) {
   return(
      6 * x + 2
   )
}

# plot function
plot(seq(-5, 5, 1e-3), objective(seq(-5, 5, 1e-3)), type = "l")


# Testing function
Newton(
   f = objective,
   f_p = objective_prime,
   f_pp = objective_sec,
   dX0 = -1
)

Newton(
    objective,
    objective_prime,
    objective_sec,
    dX0 = 4
)


# verify solution
optimize(objective, lower = -5, upper = 5, maximum = TRUE) # maximum
optimize(objective, lower = -5, upper = 5) # minimum
