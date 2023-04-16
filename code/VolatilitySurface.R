###############################################################################
#### VolatilitySurface.R #####
#
# Author:  Alessio  
# Description: Functions to compute, train and plot the volatility surface
###############################################################################
###############################################################################

#############################
### 1. Model Volatility  ####
#############################

f_sig_IV <- function(alpha, m, t){
  #m is an array of K/S
  #t is a repetitive array same tau for K
  #IV is a vector or numbers
  #alpha is a set of paramters
  a1 <- alpha[1]
  a2 <- alpha[2]
  a3 <- alpha[3]
  a4 <- alpha[4]
  
  sig <- a1 + a2*(m-1)^2 + a3*(m-1)^3 + a4*sqrt(t)
  
  return(sig)
}

##############################
### 2. Objective Function ####
##############################

f_fit <- function(alpha, m, t, IV){
  
  sig <- f_sig_IV(alpha, m, t)
  error <- abs(IV - sig)
  
  return(sum(error))
}

##############################
### 3. Model Optimization ####
##############################

f_sig_optim <- function(call_put_data){ 
  
  # parameter initialization
  start <- c(1, 1, 0, 1)
  
  # Optimize parameters using input data  
  fit <- optim(par = start,
               fn = f_fit,
               method = "BFGS",
               m = call_put_data$m,
               t = call_put_data$tau,
               IV = call_put_data$IV)
  
  # return parameters
  return(fit$par)
}

  
  