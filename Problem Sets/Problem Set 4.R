### Problem 1 ###
func <- function(x, l = 0.1, m = 15) {
    numerator <- x * ((x + 1)^m)
    denominator <- ((x + 1)^m) + 1

    return(l - (numerator / denominator))
}

bisection <- function(f, x_l, x_r, tol = 1e-10) {
    if (x_l >= x_r) {
        stop("x_l >= x_r \n")
    }

    fval_l <- f(x_l)
    fval_r <- f(x_r)
    if (fval_l == 0) {
        return(x_l)
    } else if (fval_r == 0) {
        return(x_r)
    } else if (prod(fval_l, fval_r) > 0) {
       stop("f(x_l) * f(x_r) > 0 \n")
    }

    while ((x_r - x_l) > tol) {
        x_mid <- sum(x_r, x_l) / 2
        fval_mid <- f(x_mid)

        if (fval_mid == 0) {
            return(x_mid)
        } else if (prod(fval_l, fval_mid) < 0) {
            x_r <- x_mid
            fval_r <- fval_mid
        } else {
            x_l <- x_mid
            fval_l <- fval_mid
        }
        }
    return(sum(x_l, x_r) / 2)
}

# calculate roots
root_bisection <- bisection(func, -10, 10)
root_uniroot <- uniroot(func, c(-10, 10))["root"]$root

# plotting
x <- seq(-0.5, 0.5, 1e-5)
y <- func(x)
plot(x, y, "l")
abline(h = 0, v = root_bisection)

paste("Bisection: ",
      round(root_bisection, 7),
      " Uniroot: ",
      round(root_uniroot, 7))


### Problem 2 ###
cos_minus_x <- function(x) {
   return(cos(x) - x)
}

cos_minus_x_prime <- function(x) {
   return(-sin(x) - 1)
}

log_minus_exp <- function(x) {
    return(log(x) - exp(-x))
}

log_minus_exp_prime <- function(x) {
    return((1 / x) + exp(-x))
}

secant <- function(f, x_0, x_1, tol = 1e-16, n = 1000) {
    for (i in seq(0, n)) {
        x_2 <- x_1 - f(x_1) * ((x_1 - x_0) / (f(x_1) - f(x_0)))

        if (abs(f(x_2)) < tol) {
            return(x_2)
        }

        x_0 <- x_1
        x_1 <- x_2
    }
}

newton_raphson <- function(f, f_prime, x_0, tol = 1e-16, n = 1000) {
   for (i in seq(0, n)) {
       x_1 <- x_0 - (f(x_0) / f_prime(x_0))

       if (abs(f(x_1)) <= tol) {
           return(x_1)
       }
       x_0 <- x_1
   }
   stop("Failed to converge")
}


secant(cos_minus_x, x_0 = 1, x_1 = 2)
secant(log_minus_exp, x_0 = 1, x_1 = 2)

microbenchmark::microbenchmark(
    newton_raphson(log_minus_exp, log_minus_exp_prime, x_0 = 1),
    secant(log_minus_exp, x_0 = 1, x_1 = 2), times = 100
)
microbenchmark::microbenchmark(
    newton_raphson(cos_minus_x, cos_minus_x_prime, x_0 = 1),
    secant(cos_minus_x, x_0 = 1, x_1 = 2), times = 100
)


### Problem 3 ###

func <- function(x) {
   return(-x**2 + 1)
}

goldensection <- function(f, x_l, x_m, x_r, tol = 1e-8) {
    # Golden ratio plus 1
    grto <- 1 + ((1 + sqrt(5)) / 2)

    while ((x_r - x_l) > tol) {
        # validation
        if (!(x_l < x_m & x_m < x_r)) {
            stop("Equalities unmatched -> x_l < x_m < x_r")
        }

        if ((x_r - x_m) > (x_m - x_l)) {
            y <- x_m + (x_r - x_m) / grto

            if (f(y) >= f(x_m)) {
                x_l <- x_m
                x_m <- y
            } else {
                x_r <- y
            }

        } else {
            y <- x_m - (x_m - x_l) / grto

            if (f(y) >= f(x_m)) {
                x_r <- x_m
                x_m <- y
            } else {
                x_l <- y
            }
        }
    }
    return(x_m)
}

goldensection(func, -10, 1, 10)


### Problem 4 ###

func <- function(x) {
   if (x == 0) {
       return(x)
   }

   return(abs(x) * log(abs(x) / 2) * exp(-abs(x)))
}

# plotting
x <- seq(-10, 10, 1e-5)
y <- func(x)
plot(x, y, "l")


goldensection(func, x_l = -10, x_m = -3, x_r = 10)
goldensection(func, x_l = -10, x_m = 3, x_r = 10)


### Problem 5 ###
func <- function(x) {
   return(-x**2 + 1)
}

func <- function(x) {
   if (x == 0) {
       return(x)
   }

   return(abs(x) * log(abs(x) / 2) * exp(-abs(x)))
}

goldensection_plot <- function(f, x_l, x_m, x_r, tol = 1e-8) {
    c_x_l <- c()
    c_x_m <- c()
    c_x_r <- c()
    c_y <- c()

    # Golden ratio plus 1
    grto <- 1 + ((1 + sqrt(5)) / 2)

    while ((x_r - x_l) > tol) {
        # validation
        if (!(x_l < x_m & x_m < x_r)) {
            stop("Equalities unmatched -> x_l < x_m < x_r")
        }

        if ((x_r - x_m) > (x_m - x_l)) {
            y <- x_m + (x_r - x_m) / grto

            if (f(y) >= f(x_m)) {
                x_l <- x_m
                x_m <- y
            } else {
                x_r <- y
            }

        } else {
            y <- x_m - (x_m - x_l) / grto

            if (f(y) >= f(x_m)) {
                x_r <- x_m
                x_m <- y
            } else {
                x_l <- y
            }
        }

        c_x_l <- append(c_x_l, x_l)
        c_x_m <- append(c_x_m, x_m)
        c_x_r <- append(c_x_r, x_r)
        c_y <- append(c_y, y)
    }
    return(list(x_m, c_x_l, c_x_m, c_x_r, c_y))
}

res <- goldensection_plot(func, -10, 1, 10)

maxi <- res[1]
l_maxi <- res[2]
m_maxi <- res[3]
r_maxi <- res[4]
y_maxi <- res[5]

x <- seq(-10, 10, 1e-5)
y <- func(x)
plot(x, y, "l")

lapply(l_maxi, function(x) abline(v = x, col = "Red"))
lapply(m_maxi, function(x) abline(v = x, col = "Red"))
lapply(r_maxi, function(x) abline(v = x, col = "Red"))
lapply(y_maxi, function(x) abline(v = x, col = "Blue"))
