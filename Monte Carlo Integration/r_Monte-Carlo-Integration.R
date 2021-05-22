set.seed(69)

monte_carlo_integration <- function(func, n, a, b) {
    U <- runif(n, min = a, max = b)

    I <- (b - a) * mean(func(U))

    return(I)
}

objective <- function(x) {
   # some function f, where we want to estimate integration from a to b

   return(
       1 / (1 + x**2)
   )
}

monte_carlo_integration(
    func = objective,
    n = 1e6,
    a = -1,
    b = 1
)


# verify solution
integrate(f = objective, lower = -1, upper = 1)
