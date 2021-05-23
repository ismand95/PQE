newton <- function(
                   func,
                   func_prime,
                   func_sec,
                   x_0,
                   tol = 1e-9,
                   maxiter = 1000) {
    # initialize variables
    i <- 0

    while ((abs(func_prime(x_0)) > tol) && (i < maxiter)) {
        x_0 <- x_0 - (func_prime(x_0) / func_sec(x_0))
        i <- i + 1
        cat(i, ": x =", x_0, "\n")
    }

    if (i == maxiter) {
        return(
            list(
                optimum = NULL,
                iterations = i,
                f.optimum = NULL,
                f_prime.optimum = NULL,
                f_sec.optimum = NULL,
                type = NULL,
                convergence = "Not achieved"
            )
        )
    } else {
        return(
            list(
                optimum = x_0,
                iterations = i,
                f.optimum = func(x_0),
                f_prime.optimum = func_prime(x_0),
                f_sec.optimum = func_sec(x_0),
                type = ifelse(
                    func_sec(x_0) < 0,
                    "Local maximum",
                    "Local minimum"
                ),
                convergence = "Achieved"
            )
        )
    }

    return(x_0)
}

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


# optimize function
newton(
    objective,
    objective_prime,
    objective_sec,
    x_0 = -1
)

newton(
    objective,
    objective_prime,
    objective_sec,
    x_0 = 4
)


# verify solution
optimize(objective, lower = -5, upper = 5, maximum = TRUE) # maximum
optimize(objective, lower = -5, upper = 5) # minimum
