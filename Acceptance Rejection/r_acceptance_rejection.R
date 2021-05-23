### Auxiliary functions ###

# Setting up the probability distribution for the Gamma distribution
gamma.pdf <- function(x, k, theta) {
    return(theta^k * x^(k - 1) * exp(-theta * x))
}

# Setting up the probability distributino for the Exponential function
exponential.pdf <- function(x, lambda) {
    return(lambda * exp(-lambda * x))
}

# The following function simulates by the Inversion Method the exponential distribution. 
Exponential.Simulate <- function(lambda, size = 1) {
    V <- runif(size)
    return(-1 / lambda * log(V))
}


### Simulate random variable having Gamma distribution ###
#
# Here we use vectorization so as to accelerate and the
# the function can simulate many i.i.d. random variables
# at a time
#
###


Gamma.Simulate <- function(k, theta, size = 1) {
    lambda <- theta / k
    c <- k^k * exp(-k + 1) / gamma(k)

    U <- rep(NA, size)
    Y <- rep(NA, size)
    X <- rep(NA, size)
    Unaccepted <- rep(TRUE, size)

    while (any(Unaccepted)) {
        UnacceptedCount <- sum(Unaccepted)

        U <- runif(UnacceptedCount)
        Y <- Exponential.Simulate(lambda, UnacceptedCount)

        Accepted_ThisTime <- Unaccepted[Unaccepted] &
            (U <= (gamma.pdf(Y, k, theta) / exponential.pdf(Y, lambda) / c)) # burde det ikke være gange c?

        X[Unaccepted][Accepted_ThisTime] <- Y[Accepted_ThisTime]
        Unaccepted[Unaccepted] <- !Accepted_ThisTime
    }

    return(X)
}

Gamma.Simulate(k, theta, size = 1)

### A test of the function ###

set.seed(10086)

system.time({
    X <- Gamma.Simulate(2, 1, 10^6)
})

# histogram

hist(X,
    breaks = 30, freq = FALSE,
    main = "Theoretical and simulated Gamma(2,1) density",
    col = "cornflowerblue",
    xlim = c(0, 14), ylim = c(0, 0.35),
    xlab = "x",
    cex.main = 0.8
)

xticks <- seq(0, max(X), 0.1)
lines(xticks, dgamma(xticks, 2, 1), col = "red")
legend("topright",
    legend = c("Simulated", "Theoretical"), lty = c(1, 1),
    lwd = c(5, 1), col = c("cornflowerblue", "red")
)
