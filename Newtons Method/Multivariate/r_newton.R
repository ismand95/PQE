library(numDeriv)

newton <- function(conditions, func, arguments, tol = 1e-8, maxiter = 1000) {
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
            matrix(c(f_xx, f_xy, f_yx, f_yy), 2, 2) # always symmetric
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

# newton(
#    conditions = analytical_conditions,
#    func = objective,
#    arguments = c(-2, 4)
# )