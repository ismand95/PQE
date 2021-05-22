library(numDeriv)

objective <- function(arguments) {
    # `arguments` expects an ordered vector of variables
    return((1 - arguments[1])**2 + 100 * (arguments[2] - arguments[1]**2)**2)
}


newton <- function(conditions, func, arguments, tol = 1e-8, maxiter = 10000) {
    i <- 0
    cond_eval <- conditions(func, arguments)

    # maximum from absolute value of gradients determines a optimum
    while ((max(abs(cond_eval[[2]])) > tol) && (i < maxiter)) {
        arguments <- arguments - solve(
            cond_eval[[3]],
            cond_eval[[2]]
        )

        cond_eval <- conditions(func, arguments)

        cat(i, ":", "coordinates of (x, y):", arguments, "\n")
        i <- i + 1
    }
    if (i == maxiter) {
        cat("newton failed to converge\n")
    } else {
        return(arguments)
    }
}


numerical_conditions <- function(func, arguments) {
    return(
        list(
            func(arguments),
            grad(func, arguments),
            hessian(func, arguments)
        )
    )
}


analytical_conditions <- function(func, arguments) {
    # parse args
    x <- arguments[1]
    y <- arguments[2]

    # FOC
    f_x <- 2 * (1 - x) * (-1) + 200 * (y - x^2) * (-2 * x)
    f_y <- 200 * (y - x^2)

    # SOC
    f_xx <- 2 - 400 * y + 1200 * x^2
    f_yy <- 200
    f_xy <- -400 * x
    f_yx <- -400 * x

    return(
        list(
            func(arguments),
            c(f_x, f_y),
            matrix(c(f_xx, f_xy, f_yx, f_yy), 2, 2)  # always symmetric
        )
    )
}


newton(
    conditions = numerical_conditions,
    func = objective,
    arguments = c(-2, 4)
)
newton(
    conditions = analytical_conditions,
    func = objective,
    arguments = c(-2, 4)
)