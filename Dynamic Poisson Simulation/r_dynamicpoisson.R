# Simulation from dynamic Poisson distribution
# Function that simulate T observations

set.seed(90210)

dynamic_poisson <- function(T, phi = 0.5, alpha = 0.7) {
    poi <- rep(
        NA,
        times = T
    )

    # initials
    lambda <- phi / (1 - alpha)
    poi[1] <- rpois(n = 1, lambda = lambda)

    for (t in seq(2, T)) {
        lambda <- phi + alpha * poi[t - 1]
        poi[t] <- rpois(n = 1, lambda = lambda)
    }

    return(poi)
}

dynamic_poisson(500)