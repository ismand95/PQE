library(Rcpp)
library(microbenchmark)

### Problem 1 ###
set.seed(69)

DynamicPoissonR <- function(T, alpha = 0.7, phi = 0.5) {
    lambda_func <- function(x) {
        return(phi + alpha * x)
    }

    # initial lambda
    lambda <- phi / (1 - alpha)

    # results
    Y_t <- c(rpois(1, lambda = lambda))

    for (i in seq(2, T)) {
        lambda <- lambda_func(Y_t[i - 1])
        y_t <- rpois(1, lambda = lambda)

        Y_t <- append(Y_t, y_t)
    }
    return(Y_t)
}

resultR <- DynamicPoissonR(100)
print(resultR)


### Problem 2 ###
set.seed(69)

sourceCpp("dynamic_poisson.cpp")
resultC <- DynamicPoissonC(T = 100, alpha = 0.7, phi = 0.5)

print(resultC)

microbenchmark(DynamicPoissonR(T = 100),
               DynamicPoissonC(T = 100, alpha = 0.7, phi = 0.5),
               times = 100)


### Problem 3 ###
set.seed(69)
sourceCpp("log_likelihood.cpp")
llC <- DynamicPoissonLLC(T = 100, alpha = 0.7, phi = 0.5)

print(llC)


### Problem 4 ###

# below - Jesper's solution :-)

####################################################
##############        4       ######################
####################################################

## link to the likelihood
dAvgNegLogLike_Link <- function(vPar, vY) {
  
  dPhi   <- exp(vPar[1])
  dAlpha <- exp(vPar[2])/(1.0 + exp(vPar[2]))
  
  dAvgNegLogLike <- dAvgNegLogLike(vY, dPhi, dAlpha)
  
  return(dAvgNegLogLike)
}

Estimate_DP <- function(vY) {
  
  
  dAlpha_ini   <- 0.6  ## initial value for alpha = 0.6
  dPhi_ini     <- 1.0  ## initial value for phi = 1
  
  #we apply exponential link for alpha and logit transformation for phi
  vPar = c(log(dPhi_ini), log(dAlpha_ini/(1.0 - dAlpha_ini)))
  
  ## optimize the likelihood
  optimize <- optim(vPar, dAvgNegLogLike_Link, vY = vY, method = "BFGS")
  
  ## estimated parameters
  vPar <- optimize$par
  
  # Transformations
  dPhi   <- exp(vPar[1])
  dAlpha <- exp(vPar[2]) / (1.0 + exp(vPar[2]))
  
  
  return(c("alpha" = dAlpha,
           "phi" = dPhi))
}

Estimate_DP(lSim$vY)
