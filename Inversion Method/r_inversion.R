set.seed(1337)

gumbel.simulate <- function(n, beta = 2, mu = 0.5) {
    data_sample <- runif(n, min = 0, max = 1)

    return(mu - beta * log(-log(data_sample)))
}

# Theoretical PDF: 
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
