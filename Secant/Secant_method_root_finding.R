#Secant root-finding method with example

## Secant method
secant <- function(f, dX0, dX1, dTol = 1e-9, max.iter = 1000, ...) {
  #Initial conditions
  dX_0 <- dX0
  dX_1 <- dX1
  fx0 <- f(dX_0, ...)
  fx1 <- f(dX_1, ...)
  iter <- 0
  while((abs(fx1)) > dTol && (iter < max.iter)) {
    dX_2 <- dX_1 - fx1 * (dX_0 - dX_1)/(fx0 - fx1)
    dX_0 <- dX_1
    dX_1 <- dX_2
    fx0 <- f(dX_0, ...)
    fx1 <- f(dX_1, ...)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", dX_1, "\n")
  }
  if (abs(fx1) > dTol) {
    return(list("Algoritmen" = "failed to converge", "value of x is" = dX_1))
  } else {
    return(list("Algorithm" = "converged", "value of x is" = dX_1))
  }
}

#Example 1 - a function with a root 
f1 <- function(dX) cos(dX) - dX

#Plot the function
vX1 <- seq(-5, 5, 0.01)
plot(vX1, f1(vX1), type="l")
abline(h = 0) #add horizontal line at x = 0

dRoot_sec <- secant(f1, dX0 = -5, dX1 = 5)
dRoot_sec

uniroot(f = f1, lower = -5, upper = 5)

#Example 2 - a function with no root
f2 <- function(dX) dX^2 + 5

#Plot the function
vX1 <- seq(-5, 5, 0.01)
plot(vX1, f2(vX1), type = "l")
abline(h = 0) #add horizontal line at x = 0 

dRoot_sec <- secant(f2, dX0 = -5, dX1 = 5)
dRoot_sec
