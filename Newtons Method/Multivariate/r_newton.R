library(numDeriv)

newton <- function(conditions, func, arguments, tol = 1e-4, maxiter = 1000) {
    # Function to optimize any function `func` of n arguments
    # `conditions` is a list of the function, gradient and Hessian
    # evaulated at a point - fx. (x, y)

    i <- 1
    cond_eval <- conditions(func, arguments)

    # maximum from absolute value of gradients determines a optimum
    while ((max(abs(cond_eval[[2]])) > tol) && (i < maxiter)) {
        arguments <- arguments - solve(
            cond_eval[[3]],
            cond_eval[[2]]
        )

        cond_eval <- conditions(func, arguments)

        cat(i, ":", "coordinates of arguments:", arguments, "\n")
        i <- i + 1
    }

    if (i == maxiter) {
        return(
            list(
                optimum = NULL,
                iterations = i,
                f.optimum = NULL,
                gradient.optimum = NULL,
                hess.optimum = NULL,
                type = NULL,
                convergence = "Not achieved"
            )
        )
    } else {
        list(
            optimum = arguments,
            iterations = i,
            f.optimum = cond_eval[1],
            gradient.optimum = cond_eval[2],
            hess.optimum = cond_eval[3],
            type = "Check hess for max/min/saddle",
            convergence = "Achieved"
        )
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
    f_x <- cos(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) * x - sin(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * 2
    f_y <- -cos(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) * y / 2 + sin(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * exp(y)

    # SOC
    f_xx <- -sin(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) * (4 + x^2) + cos(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) - cos(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * 4 * x
    f_xy_yx <- sin(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) * (x * y / 2 + 2 * exp(y)) + cos(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * (x * exp(y) + y)
    f_yy <- -sin(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) * (y^2 / 4 + exp(2 * y)) - cos(x^2 / 2 - y^2 / 4) * cos(2 * x - exp(y)) / 2 - cos(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * y * exp(y) + sin(x^2 / 2 - y^2 / 4) * sin(2 * x - exp(y)) * exp(y)

    return(
        list(
            func(arguments),
            c(f_x, f_y),
            matrix(c(f_xx, f_xy_yx, f_xy_yx, f_yy), 2, 2) # always symmetric
        )
    )
}


objective <- function(arguments) {
    # `arguments` expects an ordered vector of variables
    # based on slides lecture 8, part 2 - slide 13
    return(
        (sin(arguments[1]^2 / 2 - arguments[2]^2 / 4)
        * cos(2 * arguments[1] - exp(arguments[2])))
    )
}


newton(
    conditions = numerical_conditions,
    func = objective,
    arguments = c(1.6, 1.2)
)

newton(
    conditions = analytical_conditions,
    func = objective,
    arguments = c(1.6, 1.2)
)


numerical_conditions(
    objective,
    c(1.6, 1.2)
)
analytical_conditions(
    objective,
    c(1.6, 1.2)
)
