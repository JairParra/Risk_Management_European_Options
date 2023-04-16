###############################################################################
#### Options Pricing.R #####
#
# Author: Hair Parra 
# Description: Functions used for Options Pricing 
###############################################################################
###############################################################################

####################
### 0. Packages ####
####################

###############################################################################

#########################
### 1. Black-Scholes ####
#########################

get_d1 <- function(S_t, K, tau, r, sigma){ 
  ### Compute d1 for the Black-Scholes model 
  # INPUTS
  #   S_t:  Current value of underlying asset price
  #   K:    Strike Price
  #   tau:  T- t, where T=maturity, and t=current time
  #   r:    risk-free rate 
  #   sigma   Implied volatility (i.e. sigma)
  
  num <- (log(S_t/K) - tau*(r + 0.5*sigma**2)) # numerator
  denom <- sigma * sqrt(tau) # denominator 
  
  return(num/denom)
}

get_d2 <- function(d1, sigma, tau){
  ### Compute d2 for the Black-Scholes model 
  # INPUTS
  #   d1:  d1 factor calculated by the get_d1 function
  #   tau:  T- t, where T=maturity, and t=current time
  #   sigma   Implied volatility (i.e. sigma)
  
  return(d1 - sigma * sqrt(tau))
}

# Function to implement the Black-Scholes model 
black_scholes <- function(S_t, K, r, tau, sigma, put=FALSE){
  # Calculates a Call (or Option) price using Black-Scholes
  # INPUTS
  #   S_t:     [numeric] Current value of underlying asset price
  #   K:       [numeric] Strike Price
  #   r:       [numeric] risk-free rate 
  #   tau:     [numeric] T- t, where T=maturity, and t=current time
  #   sigma:   [numeric] Implied volatility (i.e. sigma)
  #   put:     [logical] if TRUE, calculate a Put, if FALSE, calculate a Call. 
  #            FALSE by default (Call). 
  # 
  # OUTPUTS: 
  #   P or C: [numeric] Option value according to Black-scholes
  
  # calculate d1 & d2
  d1 <- get_d1(S_t, K, tau, r, sigma)
  d2 <- get_d2(d1, sigma, tau)
  
  if(put==TRUE){ 
    # calculate a Put option 
    P <- K*exp(-r*tau)*(1 - pnorm(d2)) - S_t * (1 - pnorm(d1))
    P <- as.numeric(P)
    return( round(P,6))
  }
  # else calculate a Call option (default) 
  C <- S_t * pnorm(d1) - K*exp(-r*tau) * pnorm(d2)
  return( round(as.numeric(C),6) )
}

# # Test: Call Option 
# S_t = 1540
# K = 1600 
# r = 0.03
# tau = 10/360 
# sigma = 1.05
# black_scholes(S_t, K, r, tau, sigma)

###############################################################################

##########################
### 2. Option Pricing ####
########################## 

get_nearest<- function(x, vec){ 
  ### Obtain the two nearest values of x in vec. 
  
  # find all the numbers that are bigger and smaller than x in vec 
  bigger <- vec >= x 
  smaller <- vec <= x
  
  # filter only values with TRUE 
  bigger <- bigger[bigger == TRUE]
  smaller <- smaller[smaller == TRUE]
  
  # obtain the indexes for the left and upper bound
  a_idx <- length(smaller)
  b_idx <- length(smaller)+1
  
  # retrieve values from original vector 
  a <- vec[a_idx]
  b <- vec[b_idx]
  
  # return the retrieved values 
  return( c(a,b) )
}


interpolate <- function(x,x1=1,y1=1,x2=2,y2=2){
  ### Function to interpolate y given two points
  y1 + (x-x1)*(y2-y1)/(x2-x1)
}


price_option <- function(T, K, calls, rf_mat, stock=NULL, S_t=NULL, IV = NULL, put=FALSE){ 
  # Calculates the price of an European option using input parameters
  # INPUTS
  #   T:        [numeric] maturity of option (in days)
  #   K:        [numeric] Strike Price
  #   calls:    [matrix] matrix containing information about tau and IV for different strike prices
  #   rf_mat:       [matrix] matrix containing risk-free term structure
  #   stock:     [xts OR zoo like object] object containing stock prices for a single stock
  #   S_t:      [numeric] Specific price at time t 
  #   IV:    [float] Implied volatility of the underlying 
  #   put:     [logical] if TRUE, calculate a Put, if FALSE, calculate a Call. 
  #            FALSE by default (Call). 
  # 
  # OUTPUTS: 
  #   LIST containing: 
  #     - P or C: [numeric] Option value according to Black-scholes and available information
  #     - r_interp: [numeric] Interpolated risk-free rate given risk-free term structure
  #     - calls [matrix] relevant set of calls information 
  #     - rates [matrix] relevant set of risk-free rates used for the interpolation
  
  # Sanity check 
  if(!is.matrix(calls) | !('tau_days' %in% colnames(calls)) ){
    stop("calls should be a matrix with columns c('K', 'tau', 'IV', 'tau_days')")
  }
  
  # Inputs
  sigma <- NA
  tau = T/250 # days --> years 
  days_calls <- calls[,"tau_days"] # extract days column
  days_rf <- rf_mat[, "days"] # extract days from rf_mat
  
  # extract the calls values 
  ab <- get_nearest(T, days_calls) # search lower and upper nearest days to T
  valid_days <- calls[, "tau_days"] == ab[1] | calls[, "tau_days"] == ab[2] # where match
  calls_sub <- calls[ valid_days,  ] # subset valid rows
  calls_sub <- calls_sub[calls_sub[,"K"]==K, ] # subset matching K 
  
  # test whether matrix is empty (i.e. no matching K found)
  if(all(is.na(calls_sub))){
    warning("No values matching K in Calls data\n")
  }
  
  # extract interpolated risk rates 
  ab <- get_nearest(T, days_rf) # obtain nearest days to T available in rf_mat 
  valid_days_rf <- rf_mat[, "days"] == ab[1] | rf_mat[, "days"] == ab[2] # where match
  rates <- rf_mat[valid_days_rf, ] # subset for valid days 
  
  # interpolate risk free rate for Option given maturity 
  r <- interpolate(tau, 
                   x1=rates[1,2], 
                   y1=rates[1,1], 
                   x2=rates[2,2], 
                   y2=rates[2,1])
  
  # use provided sigma by default, else calculate from calls matrix
  if(is.null(IV)){
    
    # retrieve implied volatility for option 
    if(is.matrix(calls_sub)){
      # average between lower and upper values 
      sigma <- (calls_sub[1, "IV"] + calls_sub[2, "IV"])/2
      
    } else{
      # retrive from numeric vector (single match)
      sigma <- calls_sub["IV"]
    }
    
  }
  else{ 
    # rename for convenience
    sigma <- IV
  }
  
  # if price at t is not provided
  if(is.null(S_t) & !is.null(stock)){
    # retrieve last price for option from input index
    warning("Using last day's S_t from input index\n")
    S_t <- as.numeric( stock[length(stock)])
  }
  
  # Calculate Option price 
  if(put==TRUE){
    C <- NA
    P <- black_scholes(S_t, K, r, tau, sigma, put=TRUE)
  }
  else{ 
    C <- black_scholes(S_t, K, r, tau, sigma, put=FALSE)
    P <- NA
  }
  
  # pack everything into a List and return 
  return(list(Call = C, 
              Put = P, 
              S = as.numeric(S_t)[[1]], 
              K = K, 
              r_interp = r, 
              calls = calls_sub, # subset of calls used
              rates = rates # subset of rates used 
  ))
}


prc_opt <- function(T, K, calls, rf_mat, price_vec, vol_vec){ 
  #### Wrapper for price_option for two vectors of prices and volatilities 
  # 
  # INPUTS
  #   T:            [numeric] time to maturity
  #   calls:        [matrix] matrix containing information about tau and IV for different strike prices
  #   rf_mat:       [matrix] matrix containing risk-free term structure
  #   price_vec:    [numeric vector] vector of stock (sp500) prices
  #   vol_vec:      [numeric vector] vector of corresponding volatilities 
  # 
  # OUTPUTS: 
  #   opt prices: [numeric vector] vector of option prices  
  
  # abstract price_opt function two arguments: S_t and IV 
  price_opt_abstr <- function(x,y){price_option(T=T, # maturity
                                                K=K, # strike 
                                                calls, # calls matrix 
                                                rf_mat, # matrix of risk free structure
                                                stock = NA, # ignore 
                                                S_t = x, # specific price
                                                IV = y)$Call} # implied volatility + extract Call price
  
  # pack both vectors into a dataframe 
  vec_df <- data.frame(price_vec, vol_vec) 
  
  # Calculate the options for all input S_t  and corresponding volatilities
  opt_prices <- mapply(price_opt_abstr, vec_df$price_vec, vec_df$vol_vec)
  
  return(opt_prices)
}


f_opt_price_simulation <- function(sim_price_sp500 = NULL, sim_vol_vix = NULL, K_vec = NULL, T_vec = NULL, put=FALSE){
  #### Computes Option prices (premiums) based on input simulation matrices, option prices and maturities 
  # 
  # INPUTS
  #   sim_price_sp500:    [matrix] (n_sim x n_steps) matrix of simulated prices for the sp500 
  #                       .It is assumed the n_steps represented the number of days ahead simulation. 
  #   sim_vol_vix:        [matrix] (n_sim x n_steps) matrix of simulated volatilities for the vix
  #   K_vec               [vector] vector of strike prices to compute 
  #   T-vec               [vector] vector of maturities for each of the options
  # 
  # OUTPUT: 
  #   mats:       [list of matrices] List containing three matrices of compatible sizes containing the 
  #               option prices (premiums) for the corresponding prices and market volatilities (vix) 
  
  # verify that at least sp500 arguments are given 
  if(is.null(sim_price_sp500)){
    stop("sim_pricesp500 must be supplied")
  }
  
  # safety check 
  if(!is.null(sim_vol_vix) & (ncol(sim_price_sp500) != ncol(sim_vol_vix))) {
    stop("Incompatible sim_price_sp500 and sim_vol_vix matrix sizes") 
  }
  if(length(K_vec) != length(T_vec)){
    stop("Incompatible Strike vector and Maturities vector lengths")
  }
  
  # unpack values 
  n_ahead <- ncol(sim_price_sp500)
  n_K <- length(K_vec) # num of options
  
  # generate option names for the list 
  lnames <- as.vector(mapply(paste0, rep("opt", n_K), seq(1:n_K)))
  
  # Initialize empty matrices to store the simulated option prices (aka premiums)
  opt_price_mats <- initialize_sim_mats(sim_price_sp500, 
                                        lnames = lnames,
                                        num_mats = n_K
  )
  
  # looop through simulated prices (n_ahead days)
  for(t in 1:n_ahead){
    
    # extract simulated prices for sp500 at T+t
    prices_t <- sim_price_sp500[, t] 
    
    # extract implied volatility from vix at T+t 
    vols_t <- sim_vol_vix[, t]
    
    # for every available strike and corresponding maturity (i.e. option)
    for(j in 1:n_K){
      # extract T and K  
      T <- T_vec[j]
      K <- K_vec[j]
      opt_name <- lnames[j]
      
      # price first Call option
      c_vec <- prc_opt(T-t, K, calls, rf_mat, prices_t, vols_t) # vec of premiums
      opt_price_mats[[opt_name]][ ,t] <- c_vec # assign to matrix 
    }
    
  }
  
  return(opt_price_mats)
}


f_pl_simulation <- function(sim_price_sp500, opt_price_mats, K_vec, put=FALSE){
  #### Computes Profit and loss from the options(premiums) based on input simulation and option prices matrices
  # 
  # INPUTS
  #   sim_price_sp500:    [matrix] (n_sim x n_steps) matrix of simulated prices for the sp500. 
  #                       It is assumed the n_steps represented the number of days ahead simulation. 
  #   opt_price_mats:     [list of matrices] List containing three matrices of compatible sizes containing the 
  #                       option prices (premiums) for the corresponding prices and market volatilities (vix) 
  #   K_vec               [vector] vector of strike prices to compute (for each of the options)
  # 
  # OUTPUT: 
  #   PL_mats:            [list of matrices] Profit and loss matrices corresponding to each of the options 
  
  # unpack values 
  n_ahead <- ncol(sim_price_sp500) # predition days ahead 
  n_K <- length(K_vec) # num of options
  
  # generate option names for the list 
  optnames <- names(opt_price_mats) # names of each of the options matrices 
  plnames <- as.vector(mapply(paste0, rep("PL", n_K), seq(1:n_K)))
  
  # Matrices of profit and loss for each of the options simulations 
  PL_mats <- initialize_sim_mats(sim_price_sp500, 
                                 lnames = plnames,
                                 num_mats = n_K
  )
  
  # looop through simulated prices (n_ahead days)
  for(t in 1:n_ahead){
    
    #spot price of underlying at day T+t
    spot <- sim_price_sp500[, t]
    
    # for every available strike and corresponding maturity (i.e. option)
    for(j in 1:n_K){
      # extract T and K  
      K <- K_vec[j]
      pl_name <- plnames[j] # PL1, PL2, ...
      opt_name <- optnames[j] # opt1, opt2, ...
      
      # Option profit for K at time T+t with premiums c (vector)
      c <- opt_price_mats[[opt_name]][, t] # extract the premiums
      PL_mats[[pl_name]][,t] <- option_profit(S=spot, K=K_vec[j], c=c)$call_profit
    }
    
  }
  
  return(PL_mats)
}



###############################################################################

#########################
### 3. Option Profit ####
#########################

option_profit <- function(S,K,c=NULL, p=NULL, short=FALSE, N=1){
  #### Calculates Call and or Put profits
  # 
  # INPUTS
  #   S:          [numeric or vector] array of prices to use 
  #   K:          [numeric] Strike price for the option 
  #   c:          [numeric or vector] array of premiums for a Call option
  #   sim_mat:    [matrix] (n_sim x n_days_ahead) matrix of simulation prices for 
  #               n_days ahead, with n_sim simulations. 
  #   lnames:     [character vector] vector with names for each of the created matrices 
  #   num_mats:   [numeric] number of matrices to create 
  # 
  # OUTPUT: 
  #   mats:       [list of matrices] List containing three matrices of compatible sizes as sim_mat
  #               initialized to NA  values
  
  # Initialize empty profit values 
  profits <- list(call_profit=NA, put_profit=NA)
  call_profit = NA
  put_profit = NA
  
  # sanity check 
  if(is.null(c) & is.null(p)){
    stop("At least one of c or p must be provided") 
  }
  
  # if c, calculate the Call profit 
  if(!is.null(c)){
    profits$call_profit <- (max(S - K, 0) - c)*N
  }
  
  # if p, calculate the Put profit 
  if(!is.null(p)){
    profits$put_profit <- (max(K - S, 0) - p)*N
  }
  
  # inverse profit if short position 
  if(short){
    profits <- lapply(profits, function(x){-x})
  }
  
  # multiply by size 
  return(profits)
}