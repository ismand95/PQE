library(Rcpp)
library(RcppArmadillo)

sourceCpp("c_Monte-Carlo-Integration.cpp")


objective <- function(x) {
    # some function f, where we want to estimate integration from a to b

    return(
        1 / (1 + x**2)
    )
}

set.seed(69)
MonteCarloIntegration(
    func = objective,
    n = 1e6,
    a = -1,
    b = 1
)
