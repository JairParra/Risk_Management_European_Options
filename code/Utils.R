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