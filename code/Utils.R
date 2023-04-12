###############################################################################
#### Utils.R #####
#
# Author: Hair Parra 
# Description: Misc. Utility functions
###############################################################################
###############################################################################

####################
### 0. Packages ####
####################

###############################################################################

##################################
### 1. Objtect Initialization ####
##################################

initialize_sim_mats <- function(sim_mat, lnames=NULL, num_mats=3){
  #### Initializesa list of (num_mats) many matrices of same size as sim_mat with NA entries
  # 
  # INPUTS
  #   sim_mat:    [matrix] (n_sim x n_days_ahead) matrix of simulation prices for 
  #               n_days ahead, with n_sim simulations. 
  #   lnames:     [character vector] vector with names for each of the created matrices 
  #   num_mats:   [numeric] number of matrices to create 
  # 
  # OUTPUT: 
  #   mats:       [list of matrices] List containing three matrices of compatible sizes as sim_mat
  #               initialized to NA  values
  
  
  # P/L of book of options 
  mats <- lapply(rep(1, num_mats), # generate three empty matrices of compatible sizes
                 function(x){
                   matrix(NA, 
                          nrow(sim_mat), 
                          ncol(sim_mat),
                          dimnames=list(seq(1:nrow(sim_mat)),
                                        c("T+1", "T+2", "T+3", "T+4", "T+5")
                          )
                   )
                 }
  )
  
  # assign column names 
  if(!is.null(lnames)){
    names(mats) <- lnames
  }
  
  return(mats)
}

#######################################################
### 2. Price prediction from Simulated Log-returns ####
#######################################################


f_next_Pt <- function(Pt, log_Rt_next){
  #### Wrapper for price_option for two vectors of prices and volatilities 
  # 
  # INPUTS
  #   Pt:           [numeric or vector] Price of underlying at time t  (aka P_{t})
  #   log_Rt_next:  [vector numeric] vector of simulated log returns at time t+1 (aka log(R_{t+1}) )
  # 
  # OUTPUTS: 
  #   opt prices: [numeric vector] vector of prices computed from current underlying price Pt and 
  #               the log returns for the next day ahead R_{t+1}, via the formula: 
  #                P_{t+1} = exp( log(R_{t+1}) +  log(P_{t}) ) 
  
  # compute the log of Pt 
  log_Pt <- log(Pt)
  
  # P_{t+1} = exp( log(R_{t+1}) +  log(P_{t}) ) 
  Pt_next <- exp(log_Rt_next + log_Pt)
  
  return(Pt_next)
}


f_logret_to_price <- function(sp_init, vix_init, sim_rets_sp500, sim_rets_vix){ 
  #### Computes prices from log-returns using simulated returns
  # 
  # INPUTS
  #   sp_init:            [numeric] initial value for the sp500 (e.g. last  observed vallue in the index)
  #   vix_init:           [numeric] initial value for the vix(e.g. last  observed vallue in the index)
  #   sim_rets_sp500:    [numeric] (n_sim x n_steps) matrix of simulated returns 
  #   vix_init:          [numeric] initial value for the vix(e.g. last  observed vallue in the index)
  # 
  # OUTPUT: 
  #   mats:             [list of matrices] List containing two (n_sim x n_steps) matrices with 
  #                     prices (or values for the vix) computed from the log returns and initial prices.
  
  
  # Initialize empty matrices for the simulated sp500 and vix values
  sim_val_mats <- initialize_sim_mats(sim_rets_sp500, 
                                      lnames = c("sp500", "vix"), # <- this function comes from Utils.R
                                      num_mats = 2
  )
  
  # Initialize the first prices 
  sim_val_mats$sp500[, 1] <- f_next_Pt(spT, sim_rets_sp500[, 1])
  sim_val_mats$vix[, 1] <- f_next_Pt(vixT, sim_rets_vix[, 1])
  
  # for each day ahead
  for(t in 2:n_ahead){
    # obtain the values for P_{t-1}
    Pt_prev_sp500 <- sim_val_mats$sp500[, t-1] 
    Pt_prev_vix <- sim_val_mats$vix[, t-1] 
    
    # extract current returns R_{t}
    Rt_sp500 <- sim_rets_sp500[, t] 
    Rt_vix <- sim_rets_vix[, t]
    
    # compute and assign next price ahead using current returns
    sim_val_mats$sp500[, t] <- f_next_Pt(Pt_prev_sp500, Rt_sp500) 
    sim_val_mats$vix[, t] <- f_next_Pt(Pt_prev_vix, Rt_vix) 
    
  }
  
  # return forecasted values 
  return(sim_val_mats)
}

#######################
### 3. VaR and ES ####
######################

f_VaR_ES <- function(x, alpha=0.05) {
  ### Calculates the VaR and the  ES at alpha level 
  
  var <- as.numeric(quantile(x, probs = alpha))
  es <- mean(x[x< var])
  
  return(list(VaR=var, ES=es))
}