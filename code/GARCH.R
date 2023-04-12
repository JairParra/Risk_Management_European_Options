###############################################################################
#### GARCH.R #####
#
# Author: David Ardia, Statistical Methods for Financial Data 
# Description: Functions for conditional variance, maximum likelihood, and simulation from a GARCH(1,1) model
###############################################################################
###############################################################################


#################################
### 1. Conditional Variance  ####
#################################

f_sig2 <- function(theta, y) {
  
  T    <- length(y) # lenght of the series
  sig2 <- rep(NA, T) # create a vector to store the cond. vars
  sig2[1] <- theta[1] / (1 - theta[2] - theta[3]) # calculate the unconditional variance
  
  # compute subsequent cond. variances using model formula
  for (i in 2:T) {
    sig2[i] <- theta[1] + theta[2] * y[i-1]^2 + theta[3] * sig2[i-1]
  }
  
  # return 
  return(sig2)
}


###################################
### 2. Negative Log-likelihood ####
###################################

f_nll <- function(theta, y) {
  
  sig2 <- f_sig2(theta, y) # extract the cond var
  ll   <- sum(dnorm(y, mean = 0, # because lielihood
                    sd = sqrt(sig2),  # needd density of normal 
                    log = TRUE)) # compute log-likehood
  nll  <- -ll # optimizing neg log-likehood == max the likelihood of the params
  return(nll)
}


##############################
### 3. Model Optimization ####
##############################

# Optimize the variance with constraints 
f_optim_garch <- function(y) {
  
  theta0 <- c(0.1 * var(y), 0.1, 0.8) # initial parameters 
  LB     <- c(0, 1e-7, 1e-7) # lower bound limits (1e-7 for numerical stability)
  A      <- c(0, 1, 1) # stationarity condition
  b      <- 1 # 
  ui     <- rbind(c(1,0,0), c(0,1,0), c(0,0,1), -A)
  ci     <- c(LB, -b + 1e-7)
  
  # run the constrainted optimization for conditions
  optim <- constrOptim(theta = theta0, 
                       f = f_nll, 
                       grad = NULL, 
                       ui = ui, 
                       ci = ci, 
                       y = y)
  
  # extract params of interest 
  theta_hat <- optim$par # hat because estimates
  sig2_hat  <- f_sig2(theta_hat, y) # recall: sig2 is a function of the params
  eps_hat   <- y / sqrt(sig2_hat) # estimates of the innovations 
  
  out <- list(theta_hat = theta_hat, # pack into a list for convenience
              sig2_hat = sig2_hat, 
              eps_hat   = eps_hat)
  return(out)
}

############################
### 4. Model Simulation ####
############################


# Function to produce simulation 
f_sim <- function(theta, eps) {
  # Simulate sequence of returns from any theta and epsilon.
  
  T <- length(eps) # number of simulations 
  y_sim <- sig2 <- rep(NA, T) # pre-allocate space to calc the series of interest
  sig2[1]  <- theta[1] / (1 - theta[2] - theta[3]) # calculate unconditional variance
  y_sim[1] <- eps[1] * sqrt(sig2[1]) # calculate initial simulated y
  
  # continue for T-1 times recursively 
  for (i in 2:T) {
    sig2[i]  <- theta[1] + theta[2] * y_sim[i-1]^2 + theta[3] * sig2[i-1]
    y_sim[i] <- eps[i] * sqrt(sig2[i])
  }
  
  return(y_sim)
}

############################
### 5. Forecasting ####
############################


# function to forecast from previous observations
f_forecast_y <- function(theta, sig2_prev, y_prev, resids_next) {
  # Produces step-ahead values for the 
  # INPUTS
  #   theta:      [vector] fitted GARCH(1,1) model parameters
  #   sig2_prev:  [numeric] last available conditional variance to start predicting with 
  #   y_prev:     [numeric] last available observation from ground data (e.g. index, stock) 
  #   resids_next:  [vector] vector of simulated residuals from some model (e.g. Copula)
  # 
  # OUTPUTS: 
  #   y_next:   [vector] vector of forecasted y values using inputs. 
  
  
  steps <- length(resids_next)
  sig2 <- rep(NA, steps) # create a vector to store the cond. vars
  y_next <- rep(NA, steps)
  
  # calculate first conditional variance and y_forecast (given last step)
  sig2[1] <- theta[1] + theta[2] * y_prev^2 + theta[3] * sig2_prev
  y_next[1] <- resids_next[1]*sqrt(sig2[1])
  
  
  # compute subsequent cond. variances and y_nextusing model formula
  for (t in 2:steps) {
    sig2[t] <- theta[1] + theta[2] * y_next[t-1]^2 + theta[3] * sig2[t-1]
    y_next[t] <- resids_next[t]*sqrt(sig2[t])
  }
  
  # return the forecasted values and conditional variances 
  return(list(resids_next = resids_next, 
              sig2_next = sig2, 
              y_next = y_next))
}


f_forecast_x <- function(phi, x_prev, resids_next){
  # Produces step-ahead values for the AR(1) using input simulated errors 
  # INPUTS
  #   phi:      [numeric] fitted GARCH(1,1) model parameters
  #   resids_next:  [vector] vector of simulated residuals from some model (e.g. Copula)
  # 
  # OUTPUTS: 
  #   y_next:   [vector] vector of forecasted x values using inputs and the AR(1) model 
  
  # initialize 
  steps <- length(resids_next)
  x_next <- rep(NA, steps)
  
  # obtain first value 
  x_next[1] <- phi*x_prev + resids_next[1]
  
  # compute subsequent values in a loop 
  for(t in 2:steps){
    x_next[t] <- phi*x_next[t-1] + resids_next[t]
  }
  
  return(x_next)
}