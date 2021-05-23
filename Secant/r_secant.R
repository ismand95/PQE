objective <- function(x) {
    return(cos(x) - x)
}

# func without root - for testing
objective <- function(x) {
   return(x^2 + 5)
}


secant <- function(func, x_0, x_1, tol = 1e-10, maxiter = 10000, ...) {
    # initialize variables
    i <- 0

    while ((abs(func(x_1, ...)) > tol) && (i < maxiter)) {
        x_2 <- x_1 - func(x_1, ...) * ((x_0 - x_1) / (func(x_0, ...) - func(x_1, ...)))

        # swap variables
        x_0 <- x_1
        x_1 <- x_2

        # increment
        i <- i + 1

        cat("At iteration", i, "value of x is:", x_1, "\n")
    }

    if (abs(func(x_1, ...)) > tol) {
        return(
            list(
                iterations = i,
                convergence = "Not achieved",
                root = NULL,
                func.root = NULL
            )
        )
    } else {
        return(
            list(
                iterations = i,
                convergence = "Achieved",
                root = x_1,
                func.root = func(x_1)
            )
        )
    }
}

# find root
secant(objective, x_0 = 1, x_1 = 2)

# Plot the function
plot(seq(-5, 5, 1e-3), objective(seq(-5, 5, 1e-3)), type = "l")
abline(h = 0, col = "red")

# verify solution
uniroot(objective, interval = c(-5, 5))
