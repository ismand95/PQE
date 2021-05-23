objective <- function(x, m, l) {
    return(l - ((x * (x + 1)**m) / ((x + 1)**m + 1)))
}


bisection <- function(func, x_l, x_r, tol = 1e-5, ...) {
    # test conditions for algorithm
    if (x_l >= x_r) {
        stop("x_l !< x_r \n")
    }

    if (func(x_l, ...) * func(x_r, ...) > 0) {
        stop("f(x_l) * f(x_r) > 0 \n")
    }

    iter <- 1
    while ((x_r - x_l) > tol) {
        x_m <- (x_r + x_l) / 2
        f_x_m <- func(x_m, ...)

        if (f_x_m == 0) {
            return(
                list(
                    iterations = iter,
                    convergence = "Achieved",
                    root = x_m,
                    func.root = f_x_m
                )
            )
        } else if (func(x_l, ...) * func(x_m, ...) < 0) {
            x_r <- x_m
        } else {
            x_l <- x_m
        }
        iter <- iter + 1
        cat(iter, ":", x_l, "< root <", x_r, "\n")
    }
    return(
        list(
            iterations = iter,
            convergence = "Achieved",
            root = (x_r + x_l) / 2,
            func.root = func(((x_r + x_l) / 2), ...)
        )
    )
}

# calculate roots
bisection(objective, -10, 10, m = 15, l = 0.1)

# plot function
plot(
    seq(-10, 10, 1e-3),
    objective(seq(-10, 10, 1e-3), m = 15, l = 0.1),
    type = "l"
)
abline(h = 0, col = "red")

# verify results
uniroot(objective, interval = c(-10, 10), l = 0.1, m = 15)