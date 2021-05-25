library(Rcpp)

### Simulate GARCH(1,1)
# If it is supposed to be simulated in c++
sourceCpp("SimGarch.cpp")

# If it is supposed to be simulated in r
Sim_garchR <- function(iT, dOmega, dAlpha, dBeta) {
  vSigma2 <- numeric(iT)
  vY <- numeric(iT)

  vSigma2[1] <- dOmega / (1.0 - dAlpha - dBeta)
  vY[1] <- rnorm(1, sd = sqrt(vSigma2[1]))

  for (t in 2:iT) {
    vSigma2[t] <- dOmega + dAlpha * vY[t - 1]^2 + dBeta * vSigma2[t - 1]
    vY[t] <- rnorm(1, sd = sqrt(vSigma2[t]))
  }

  return(list(vY = vY, vSigma2 = vSigma2))
}


iT <- 10000
dOmega <- 0.1
dAlpha <- 0.05
dBeta <- 0.94

lSim <- GarchSim(iT, dOmega, dAlpha, dBeta)
plot(lSim[[1]], type = "l", ylab = "Conditional variance", xlab = "Time")
plot(lSim[[2]], type = "l", ylab = "Log returns", xlab = "Time")

### (2) - ESTIMATION ###  ------------------------------------------------------

## If the avg neg LL is to be found in c++
sourceCpp("dAvgnegLL.cpp")
# 2.a)

Estimate_Garch <- function(vY) {

  # Get starting values for parameters
  vPar <- GetStartingParameters(vY)

  # Map to unconstrained parameters
  vPar_tilde <- To_tilde(vPar)

  # Optimize the negative average log likelihood
  optimize <- optim(vPar_tilde, AvgNegLLK_Link, vY = vY, method = "BFGS")

  ## Extract estimated working (uncontrained) parameters
  vPar_tilde <- optimize$par

  # Transform to natural (constrained) parameters
  vPar <- From_tilde(vPar_tilde)

  ## Variance filtration
  vSigma2_hat <- GJR_Garch_Filter(vY,
    dOmega = vPar[1],
    dAlpha = vPar[2],
    dBeta  = vPar[3]
  )$vSigma2

  # Return estimated parameters and estimated volatility process
  lOut <- list("vPar" = vPar, "vSigma2_hat" = vSigma2_hat)

  return(lOut)
}

GetStartingParameters <- function(vY) {
  vPar <- numeric(3)

  vPar[2] <- 0.04 # alpha
  vPar[3] <- 0.90 # beta

  vPar[1] <- var(vY) * (1 - vPar[2] - vPar[3]) # omega

  return(vPar)
}

To_tilde <- function(vPar) {
  ## Inverse of the mapping function for the parameters of the model
  ## This function maps the natural parameters to the working parameters

  dOmega <- vPar[1]
  dAlpha <- vPar[2]
  dBeta <- vPar[3]

  dOmega_tilde <- log(dOmega)

  ## tecnical conditions
  dLowerLimit <- 1e-4
  dUpperLimit <- 1 - 1e-4

  dAlpha_tilde <- ModLogTransform_Inv(dAlpha, dLowerLimit, dUpperLimit)
  dBeta_tilde <- ModLogTransform_Inv(dBeta, dLowerLimit, dUpperLimit - dAlpha)

  vPar_tilde <- c(dOmega_tilde, dAlpha_tilde, dBeta_tilde)

  return(vPar_tilde)
}

ModLogTransform_Inv <- function(dX, dL, dU) {
  ## Inverse of the modified logistic transformation
  ## This function maps [L, U] in R

  dOut <- log((dX - dL) / (dU - dX))

  return(dOut)
}

From_tilde <- function(vPar_tilde) {
  ## Mapping function for the parameters of the model
  ## This function maps the working parameters to the natural parameters

  dOmega_tilde <- vPar_tilde[1]
  dAlpha_tilde <- vPar_tilde[2]
  dBeta_tilde <- vPar_tilde[3]

  dOmega <- exp(dOmega_tilde)
  ## technical conditions
  dLowerLimit <- 1e-4
  dUpperLimit <- 1 - 1e-4

  dAlpha <- ModLogTransform(dAlpha_tilde, dLowerLimit, dUpperLimit)
  dBeta <- ModLogTransform(dBeta_tilde, dLowerLimit, dUpperLimit - dAlpha)

  vPar <- c(dOmega, dAlpha, dBeta)
  return(vPar)
}

ModLogTransform <- function(dX_tilde, dL, dU) {
  ## Modified logistic transformation
  # This function maps R in [L, U]

  dOut <- dL + (dU - dL) / (1 + exp(-dX_tilde))

  return(dOut)
}

AvgNegLLK_Link <- function(vPar_tilde, vY) {
  ## Negative Average log likelihood parameterized in terms of the
  ## working parameters

  ## natural parameters (constrained)
  vPar <- From_tilde(vPar_tilde)

  ## likelihood
  dLLK <- GJR_Garch_Filter(vY,
    dOmega = vPar[1],
    dAlpha = vPar[2],
    dBeta  = vPar[3]
  )$dLLK

  ## return the negative average log likelihood
  return(-dLLK / length(vY))
}


GJR_Garch_Filter <- function(vY, dOmega, dAlpha, dGamma, dBeta) {
  iT <- length(vY)

  vSigma2 <- numeric(iT)

  vSigma2[1] <- dOmega / (1.0 - dAlpha - dBeta)
  dLLK <- dnorm(vY[1], sd = sqrt(vSigma2[1]), log = TRUE)

  for (t in 2:iT) {
    vSigma2[t] <- dOmega + dAlpha * vY[t - 1]^2 + dBeta * vSigma2[t - 1]
    dLLK <- dLLK + dnorm(vY[t], sd = sqrt(vSigma2[t]), log = TRUE)
  }

  return(list(
    dLLK = dLLK,
    vSigma2 = vSigma2
  ))
}


# 2.b)

vY <- lSim$vY
lFit <- Estimate_Garch(vY)

true <- c(0.1, 0.05, 0.94)
estimated <- lFit$vPar
compare_par <- matrix(c(true, estimated), nrow = 3, ncol = 2)
rownames(compare_par) <- c("dOmega", "dAlpha", "dBeta")
colnames(compare_par) <- c("True", "Estimated")
compare_par

# 2.c)

plot(lSim$vSigma2, type = "l", main = "Volatility Process", xlab = "time", ylab = "")
lines(lFit$vSigma2_hat, col = "cornflowerblue")
