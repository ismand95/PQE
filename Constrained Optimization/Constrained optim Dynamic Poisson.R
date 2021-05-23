## This simulates observations from a dynamic poisson model and performs a
## constrained optimization problem as to maximize the negative average LL
## by imposing some parameter constraints and transformation the problem.
## The parameter constrains is 0<alpha<1 and phi>0, and thus if different
## constraints are given, one has to correct the transformations and links
## accordingly.

library(Rcpp)
library(microbenchmark)
set.seed(69)

################## SIMULATION#################
# Simulation in R
PoisSimulate_R <- function(
                           iT,
                           dPhi,
                           dAlpha) {
  # Initialize variables and sets the first element of vLambda & vY
  vY <- numeric(iT)
  vLambda <- numeric(iT)

  vLambda[1] <- dPhi / (1 - dAlpha)
  vY[1] <- rpois(1, vLambda[1])

  for (t in 2:iT) {
    vLambda[t] <- dPhi + dAlpha * vY[t - 1]
    vY[t] <- rpois(1, vLambda[t])
  }

  return(list(vY = vY, vLambda = vLambda))
}

# Defining the true parameters and number of observations
dPhi <- 0.5
dAlpha <- 0.7
iT <- 10000

lSim <- PoisSimulate_R(iT, dPhi, dAlpha)

# Simulation in C++ and comparison of computational time
sourceCpp("PoisSimulateC.cpp")
microbenchmark(PoisSimulate_R(iT, dPhi, dAlpha), PoisSimulate_cpp(iT, dPhi, dAlpha))
lSim2 <- PoisSimulate_cpp(iT, dAlpha, dPhi)

################## ESTIMATION#################
# Likelihood in R
dAvgNegLogLikeR <- function(
                            vY,
                            dPhi,
                            dAlpha) {
  # Initialize variables and the first element of vLambda and dLLK
  iT <- length(vY)
  vLambda <- numeric(iT)
  dSumLL <- 0
  dNegAvgLL <- 0

  vLambda[1] <- dPhi / (1 - dAlpha)
  dLLK <- dpois(vY[1], vLambda[1], log = TRUE)

  for (t in 2:iT) {
    vLambda[t] <- dPhi + dAlpha * vY[t - 1]
    dLLK <- dLLK + dpois(vY[t], vLambda[t], log = TRUE)
  }

  dNegAvgLL <- -1 * (dLLK / iT)

  return(dNegAvgLL)
}

# Likelihood in C++
sourceCpp("dAvgNegLogLikeC.cpp")

# Link to the likelihood (Transforms the parameters back to re-use the old likelihood)
dAvgNegLogLike_Link <- function(
                                vPar,
                                vY) {
  dPhi <- exp(vPar[1])
  dAlpha <- exp(vPar[2]) / (1.0 + exp(vPar[2]))

  dAvgNegLogLike <- dAvgNegLogLike(vY, dPhi, dAlpha)

  return(dAvgNegLogLike)
}

Estimate_DP <- function(
                        vY) {
  dAlpha_ini <- 0.6 ## initial value for alpha = 0.6
  dPhi_ini <- 1.0 ## initial value for phi = 1

  # Applying the exponential link for alpha and logit transformation for phi
  vPar <- c(log(dPhi_ini), log(dAlpha_ini / (1.0 - dAlpha_ini)))

  ## Optimize the likelihood (The unconstrained problem)
  optimize <- optim(vPar, dAvgNegLogLike_Link, vY = vY, method = "BFGS")

  ## Obtain the estimated parameters (Transformed)
  vPar <- optimize$par

  # Transformations
  dPhi <- exp(vPar[1])
  dAlpha <- exp(vPar[2]) / (1.0 + exp(vPar[2]))


  return(c(
    "alpha" = dAlpha,
    "phi" = dPhi
  ))
}

Estimate_DP(lSim$vY)
