set.seed(1337)

# Inversion method
# Template for inverse
inversion_method <- function(iT, #write variables) {
   # iT is the number of draws we wish from the uniform distribution

    vU <- runif(iT) # drawing from uniform distribution
    # dOut <- 'write code here' # Here we provide the inverse/quantile function

    return(dOut)
}


# Inversion method with Gumbel distribution 
gumbel.simulate <- function(n, beta = 2, mu = 0.5) {
    data_sample <- runif(n, min = 0, max = 1)

    return(mu - beta * log(-log(data_sample)))
}

# Theoretical PDF
gumbel.density <- function(x, beta = 2, mu = 0.5) {
    return(
        (1 / beta) * exp(-((x - mu) / beta + exp(-(x - mu) / beta)))
    )
}

gumbel_sim <- gumbel.simulate(1e+4)
xlim <- c(min(gumbel_sim), max(gumbel_sim))

x_density <- seq(xlim[1], xlim[2], 1e-4)
gumbel_density <- gumbel.density(x_density)

# Plotting both the simulated and theoretical results to compare 
hist(
    gumbel_sim,
    breaks = 141,
    xlim = xlim,
    freq = FALSE,
    main = "Theoretical and simulated Gumbel distribution"
)
lines(x_density, gumbel_density, col = "red", lwd = 2)
