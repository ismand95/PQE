library(Rcpp)
library(microbenchmark)

#################
### Problem 1 ###
#################

### a)
set.seed(420)

### b)
sourceCpp("garch.cpp") # verified with solution

### c)
i_t <- 1000
d_omega <- 0.2
d_alpha <- 0.01
d_gamma <- 0.1
d_beta <- 0.92

set.seed(420)
sim_res <- GarchSimulation(
    iT = i_t,
    dOmega = d_omega,
    dAlpha = d_alpha,
    dGamma = d_gamma,
    dBeta = d_beta
)

### d)
plot(
    seq(1, i_t),
    sim_res$vS,
    type = "l",
    ylab = "Conditional variance",
    xlab = "Time"
)
abline(
    h = d_omega / (1 - d_alpha - (d_gamma / 2) - d_beta),
    col = "red"
)
title("Conditional variances X time")

plot(
    seq(1, i_t),
    sim_res$vY,
    type = "l",
    ylab = "Log returns",
    xlab = "Time"
)
title("Log returns X time")


#################
### Problem 2 ###
#################

### a)
indicator <- function(x) {
    if (x < 0) {
        return(1)
    } else {
        return(0)
    }
}

starting_parameters <- function(observations) {
    parameters <- list() # placeholder list for parameters

    parameters["alpha"] <- 0.05
    parameters["gamma"] <- 0.07
    parameters["beta"] <- 0.9

    # solve for omega, and estimate from variance on Y timeseries
    parameters["omega"] <- var(observations) * (1 - parameters$alpha -
        0.5 * parameters$gamma - parameters$beta)

    # convert list to numeric vector
    parameters <- as.numeric(parameters)

    return(parameters)
}

parameters_from_tilde <- function(tilde) {
    # zero and computer - not best friends
    lower_limit <- 1e-4
    upper_limit <- 1 - 1e-4

    parameters <- numeric(4)

    # retransform omega
    parameters[4] <- exp(tilde[4])

    # retransform alpha
    parameters[1] <- mod_log(
        param = tilde[1],
        low = lower_limit,
        up = upper_limit
    )

    # retransform gamma
    parameters[2] <- mod_log(
        param = tilde[2],
        low = lower_limit,
        up = 2 * (upper_limit - parameters[1])
    )

    # retransform beta
    parameters[3] <- mod_log(
        param = tilde[3],
        low = lower_limit,
        up = upper_limit - parameters[1] - 0.5 * parameters[2]
    )

    return(parameters)
}

parameters_to_tilde <- function(parameters) {
    # zero and computer - not best friends
    lower_limit <- 1e-4
    upper_limit <- 1 - 1e-4

    tilde <- numeric(4)

    # alpha must be between 0 and 1
    # otherwise denominator will be negative -> negative variance
    tilde[1] <- inv_mod_log(
        param = parameters[1],
        low = lower_limit,
        up = upper_limit
    )

    # gamma must be between 0 and 2*(1-alpha)
    # otherwise denominator will be negative -> negative variance
    tilde[2] <- inv_mod_log(
        param = parameters[2],
        low = lower_limit,
        up = 2 * (upper_limit - parameters[1])
    )

    # beta must be between 0 and (1-alpha-0.5*gamma)
    # otherwise denominator will be negative -> negative variance
    tilde[3] <- inv_mod_log(
        param = parameters[3],
        low = lower_limit,
        up = upper_limit - parameters[1] - 0.5 * parameters[2]
    )

    # omega should be between 0 and infinity
    tilde[4] <- log(parameters[4])

    return(tilde)
}

mod_log <- function(param, low, up) {
    # Modified logistic transformation
    # This function maps R in [L, U]

    param_transformed <- low + (up - low) / (1 + exp(-param))

    return(param_transformed)
}

inv_mod_log <- function(param, low, up) {
    # Inverse of the modified logistic transformation
    # This function maps [L, U] in R
    param_transformed <- log((param - low) / (up - param))

    return(param_transformed)
}


avg_neg_ll_link <- function(tilde, observations) {
    # Negative log likelihood parameterized in terms of
    # the working parameters

    # natural parameters
    parameters <- parameters_from_tilde(tilde)

    # LL
    ll <- garch_filter(
        observations = observations,
        alpha = parameters[1],
        gamma = parameters[2],
        beta = parameters[3],
        omega = parameters[4]
    )$LogLikelihood

    return(-ll / length(observations))
}


garch_filter <- function(observations, alpha, gamma, beta, omega) {
    t_g <- length(observations)

    sigma_est <- numeric(t_g)

    sigma_est[1] <- omega / (1.0 - alpha - 0.5 * gamma - beta)

    log_likelihood <- dnorm(
        observations[1],
        sd = sqrt(sigma_est[1]),
        log = TRUE
    )

    for (i in seq(2, t_g)) {
        sigma_est[i] <- omega +
            (alpha + gamma * indicator(observations[i - 1])) *
                observations[i - 1]**2 + beta * sigma_est[i - 1]

        ll_norm <- dnorm(
            observations[i],
            sd = sqrt(sigma_est[i]),
            log = TRUE
        )
        log_likelihood <- log_likelihood + ll_norm
    }

    return(list(LogLikelihood = log_likelihood, Sigma = sigma_est))
}

estimate_garch <- function(observations) {
    # get starting params
    parameters <- starting_parameters(observations = observations)

    # Map to unconstrained parameters
    tilde <- parameters_to_tilde(parameters)

    # Optimize the negative average log likelihood
    optimize <- optim(
        tilde,
        avg_neg_ll_link,
        observations = observations,
        method = "BFGS"
    )

    # extract optimized parameters
    op_tilde <- optimize$par

    # transform back to natural values
    op_parameters <- parameters_from_tilde(op_tilde)

    # run model on optimal parameters
    sigma_hat <- garch_filter(
        observations,
        alpha = parameters[1],
        gamma = parameters[2],
        beta = parameters[3],
        omega = parameters[4]
    )

    # parameters
    op_parameters <- list(
        "alpha" = op_parameters[1],
        "gamma" = op_parameters[2],
        "beta" = op_parameters[3],
        "omega" = op_parameters[4]
    )

    # returns parameters and estimated
    results <- list(
        "OptimalParameters" = op_parameters,
        "SigmaEstimate" = sigma_hat$Sigma
    )

    return(results)
}

### b)
optimal <- estimate_garch(sim_res$vY)

### c)
plot(
    y = optimal$SigmaEstimate,
    x = seq(1, i_t),
    type = "l",
    ylab = "Conditional variance",
    xlab = "Time"
)
lines(
    sim_res$vS,
    type = "l",
    col = "red"
)

legend(800, 40,
    legend = c("Simulated", "Estimated"),
    col = c("red", "black"), lty = 1, cex = 0.8
)


#################
### Problem 3 ###
#################
set.seed(420)
i_b <- 100

mc_sim <- matrix(data = 0, nrow = i_t, ncol = i_b)
mc_par <- matrix(data = 0, nrow = 4, ncol = i_b)

for (i in seq(1, i_b)) {
    simulated_series <- GarchSimulation(
        iT = i_t,
        dOmega = d_omega,
        dAlpha = d_alpha,
        dGamma = d_gamma,
        dBeta = d_beta
    )

    mc_sim[, i] <- simulated_series$vY

    estimated_parameters <- estimate_garch(mc_sim[, i])$OptimalParameters
    mc_par[1, i] <- estimated_parameters$alpha
    mc_par[2, i] <- estimated_parameters$gamma
    mc_par[3, i] <- estimated_parameters$beta
    mc_par[4, i] <- estimated_parameters$omega
}

# Plot the results
par(mfrow = c(2, 2))

hist(mc_par[1, ], main = "Alpha", nclass = 20, xlab = "")
abline(v = d_alpha, col = "red", lwd = 2)

hist(mc_par[2, ], main = "Gamma", nclass = 20, xlab = "")
abline(v = d_gamma, col = "red", lwd = 2)

hist(mc_par[3, ], main = "Beta", nclass = 20, xlab = "")
abline(v = d_beta, col = "red", lwd = 2)

hist(mc_par[4, ], main = "Omega", nclass = 20, xlab = "")
abline(v = d_omega, col = "red", lwd = 2)
