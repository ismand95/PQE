objective <- function(x) {
    if (x == 0) {
        return(0)
    } else {
        return(abs(x) * log(abs(x) / 2) * exp(-abs(x)))
    }
}

goldensection <- function(func, x_l, x_m, x_r, tol = 1e-8, ...) {
    # conditions for algorithm
    if (x_l >= x_m) {
        stop("Initial condition error: x_l >= x_m\n")
    }
    if (x_m >= x_r) {
        stop("Initial condition error: x_m >= x_r\n")
    }
    if (func(x_l, ...) > func(x_m, ...)) {
        stop("Initial condition error: func(x_l) > func(x_m)\n")
    }
    if (func(x_r, ...) > func(x_m, ...)) {
        stop("Initial condition error: func(x_r) > func(x_m)\n")
    }

    # golden ratio
    rho <- (1 + sqrt(5)) / 2

    # iteration variable
    iter <- 0

    while ((x_r - x_l) > tol) {

        if ((x_r - x_m) > (x_m - x_l)) {
            y <- x_m + (x_r - x_m) / (1 + rho)

            if (func(y, ...) >= func(x_m, ...)) {
                x_l <- x_m
                x_m <- y
            } else {
                x_r <- y
            }
        } else {
            y <- x_m - (x_m - x_l) / (1 + rho)

            if (func(y, ...) >= func(x_m, ...)) {
                x_r <- x_m
                x_m <- y
            } else {
                x_l <- y
            }
        }

        cat(iter, ":", x_m, "\n")
        iter <- iter + 1
    }
    return(x_m)
}


goldensection(objective, x_l = 0.5, x_m = 4, x_r = 10)
