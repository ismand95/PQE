
# Simulation from dynamic Poisson distribution
# Function that simulate T observations: 
set.seed(69)
poisson <- function(iT, dPhi = 0.5, dAlpha = 0.7) {
    # Container
    vY <- rep(0, iT)
    # Defining first value of lambda
    lambda_1 = dPhi / (1 - dAlpha)
    # Setting the first entry
    vY[1] <- rpois(1, lambda_1)
    
    # Setting up the loop 
    for (i in 2:iT) {
       vY[i] <- rpois(1, (dPhi + dAlpha * vY[i-1]))
    }

    return(vY)
}
# Testing function
vY <- poisson(100)

