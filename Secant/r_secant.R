objective <- function(x) {
    return(cos(x) - x)
}

secant <- function(func, x_0, x_1, tol = 1e-10, maxiter = 10000, ...) {
    for (i in seq(0, maxiter)) {
        x_2 <- x_1 -
            func(x_1, ...) * ((x_0 - x_1) / (func(x_0, ...) - func(x_1, ...)))

        if (abs(func(x_2, ...)) < tol) {
            return(x_2)
        }

        x_0 <- x_1
        x_1 <- x_2
    }
    stop("Convergence not achieved\n")
}

secant(objective, x_0 = 1, x_1 = 2)