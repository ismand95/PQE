set.seed(69)

Discrete.Simulate <- function(F, size, ...) {
    # F - Cumulative distribution function
    # size - number of i.i.d. random variables to be simulated

    m <- 0
    U <- runif(size)
    X <- rep(NA, size)
    X[F(0, ...) >= U] <- 0

    while (any(F(m, ...) < U)) {
        m <- m + 1
        X[(F(m, ...) >= U) & (F(m - 1, ...) < U)] <- m
    }

    return(X)
}


# cumulative distribution function
F <- function(x) {
    return(0.2 * ((x >= 1) && (x < 2)) + 0.5 * ((x >= 2) && (x < 3)) + (x >= 3))
}

# draw 1000 i.i.d. random variables whose CDF is F
X <- Discrete.Simulate(F, 1000)


# histogram
h <- hist(X, breaks = seq(0.55, 3.55, 0.1))
h$density <- h$counts / 1000
plot(h,
    freq = FALSE, col = "cornflowerblue",
    main = "",
    xlab = "n", ylab = "Percentage"
)