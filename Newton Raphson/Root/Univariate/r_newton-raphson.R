objective <- function(x) {
    return(cos(x) - x)
}

objective_prime <- function(x) {
    return(-sin(x) - 1)
}

newton_raphson <- function(func,
                           func_p,
                           x_0,
                           tol = 1e-10,
                           maxiter = 10000,
                           ...) {
    iter <- 0

    while ((iter < maxiter) && (abs(func(x_0, ...)) > tol)) {
        x_0 <- x_0 - (func(x_0, ...) / func_p(x_0, ...))
        iter <- iter + 1

        # comment out to ignore outputs
        cat(iter, ":", "x =", x_0, "\n")
    }

    if (abs(func(x_0, ...)) < tol) {
        return(
            list(
                root = x_0,
                iterations = iter,
                value_at_root = func(x_0),
                convergence = "Achieved"
            )
        )
    } else {
        return(
            list(
                root = NULL,
                iterations = iter,
                value_at_root = NULL,
                convergence = "Not achieved"
            )
        )
    }
}

newton_raphson(objective, objective_prime, x_0 = 1)

# verify solution
uniroot(objective, interval = c(-100, 100))
