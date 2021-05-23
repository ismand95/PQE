###Steepest Ascent Method###

# Overview
# Golden Section function
# Line search function
# Steepest ascent function
# Function (example)
# Gradient
# Verify solution with "optim"

#load from tutorial
gsection <- function(f, dX.l, dX.r, dX.m, dTol = 1e-9) { #goldensection function
  
  # golden ratio plus one
  dGR1 <- 1 + (1 + sqrt(5))/2
  
  # successively refine x.l, x.r, and x.m
  f.l <- f(dX.l)
  f.r <- f(dX.r)
  f.m <- f(dX.m)
  while ((dX.r - dX.l) > dTol) {
    if ((dX.r - dX.m) > (dX.m - dX.l)) {
      dY <- dX.m + (dX.r - dX.m)/dGR1
      f.y <- f(dY)
      if (f.y >= f.m) {
        dX.l <- dX.m
        f.l <- f.m
        dX.m <- dY
        f.m <- f.y
      } else {
        dX.r <- dY
        f.r <- f.y
      }
    } else {
      dY <- dX.m - (dX.m - dX.l)/dGR1
      f.y <- f(dY)
      
      if (f.y >= f.m) {
        dX.r <- dX.m
        f.r <- f.m
        dX.m <- dY
        f.m <- f.y
      } else {
        dX.l <- dY
        f.l <- f.y
      }
    }
  }
  return(dX.m)
}

#line search
line.search <- function(f, vX, vY, dTol = 1e-9, dA.max = 2^5) { #vX is the current point of iteration, vY corresponds to vector of derivatives at the current point vX 
  # f is a real function that takes a vector of length d
  # x and y are vectors of length d
  # line.search uses gsection to find a >= 0 such that
  # g(a) = f(x + a*y) has a local maximum at a,
  # within a tolerance of tol
  # if no local max is found then we use 0 or a.max for a
  # the value returned is x + a*y
  if (sum(abs(vY)) == 0) return(vX) # g(a) constant - in this point, we check if the gradient is equal to 0, if this is the case, we have already reached the extreme point and we just return the initial point
  g <- function(dA) return(f(vX + dA*vY)) #the function g gives us the value of the function in the next iteration for a given alpha (dA)
  # find a triple a.l < a.m < a.r such that
  # g(a.l) <= g(a.m) and g(a.m) >= g(a.r)
  # a.l - start from the left
  dA.l <- 0 #the lower bound is 0
  g.l <- g(dA.l) #we evaluate the function at 0
  # a.m - now we look at the middle point
  dA.m <- 1 #in this example we choose 1, but is could be any point below the max alpha
  g.m <- g(dA.m) #choose middle point and evaluate here
  while ((g.m < g.l) & (dA.m > dTol)) { #while loop checks if middle point is smaller than left point (we want it to be larger)
    dA.m <- dA.m/2 #we take our middle point and move it closer to our left point, and save the point
    g.m <- g(dA.m) #save value of function 
  } # the loop continue until middle point is larger than left point. 
  # if a suitable a.m was not found then use 0 for a (we use the lower bound for a which is 0)
  if ((dA.m <= dTol) & (g.m < g.l)) return(vX)
  # a.r - same for the right side 
  dA.r <- 2*dA.m #now we do the same for right point until middle point is larger than right point
  g.r <- g(dA.r) #our initial point of right side dA.r is evaluated at the function
  while ((g.m < g.r) & (dA.r < dA.max)) {
    dA.m <- dA.r #moving midpoint further to the right
    g.m <- g.r #We overwrite the function value
    dA.r <- 2*dA.m #creates new right point
    g.r <- g(dA.r) #function at new point
  } #keep iterating and stops if there exists a middle point larger than the right
  # if a suitable a.r was not found then use a.max for a
  if ((dA.r >= dA.max) & (g.m < g.r)) return(vX + dA.max*vY) #we move as far as possible in the steepest direction
  # apply golden-section algorithm to g to find a
  dA <- gsection(g, dA.l, dA.r, dA.m)
  return(vX + dA*vY)
}

#ascent function (steepest ascent function)
ascent <- function(f, grad.f, vX0, dTol = 1e-9, n.max = 100) { #iterates over the line search function
  vX.old <- vX0 #save initial starting value
  vX <- line.search(f, vX0, grad.f(vX0)) #gradient evaluated in starting point - this provides us with our first guess
  n <- 1
  while ((f(vX) - f(vX.old) > dTol) & (n < n.max)) { #if function value has not changed much between current and old, the iterations will stop - will also stop if we reach max number of iterations
    vX.old <- vX
    vX <- line.search(f, vX, grad.f(vX))
    cat("at iteration", n, "the coordinates of x are", vX, "\n")
    n <- n + 1
  }
  return(vX) #vX gives us the point at which the maximum was reached
}

## Example ## (from slide 9 in lecture 8 - multivariate optimization)
#function
f <- function(vX) {
  dOut = sin(vX[1]^2/2 - vX[2]^2/4) * cos(2*vX[1] - exp(vX[2]))
  return(dOut)
}

#gradient
grad.f <- function(vX) { #because the derivatives are quite large, they are written in two lines each. The output will therefore only be a two dimension vector
  dX_prime.1 = cos(vX[1]^2/2 - vX[2]^2/4)*vX[1] * cos(2*vX[1] - exp(vX[2]))
  dX_prime.2 = sin(vX[1]^2/2 - vX[2]^2/4) * (-sin(2*vX[1] - exp(vX[2]))*2)
  dY_prime.1 = cos(vX[1]^2/2 - vX[2]^2/4)*(-vX[2]/2) * cos(2*vX[1] - exp(vX[2]))
  dY_prime.2 = sin(vX[1]^2/2 - vX[2]^2/4) * sin(2*vX[1] - exp(vX[2]))*exp(vX[2])
  vOut = c(dX_prime.1 + dX_prime.2, dY_prime.1 + dY_prime.2)
  return(vOut)
}

#check - we can compare numerical and analytical solution
library(numDeriv)
grad(f, c(0, 0.5)) #numerical gradient
grad.f(c(0, 0.5)) #analytical gradient - we see that this and above line gives the same result

#run algorithm
ascent(f, grad.f, vX0 = c(1.6, 1.2))
#Above code completes the steepest ascent method

#Check if the escent function works by comparing to the solution found using "optim". 
### optim function ###
optim(c(1.6, 1.2), f, gr = grad.f, method = "BFGS", # the optim function takes the following input: c is the parameter vector, function, gradient, method and number of control parameter 
      control = list(fnscale = -1)) #you can change the scaling of the function, here we multiply the function with -1 - by which we find the max and not min, because by default in finds min

