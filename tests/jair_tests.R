
## parameters
Pt <- sp500[length(sp500)][[1]] # initial price P_t
log_Pt <- log(Pt) # log of current prices log(P_{t})
log_Rt_next <- sim_rets_sp500[, 1] # next set of returns log(R_{t+1})
Pt_next <- exp(log_Rt_next + log_Pt)

# display
Pt
log_Pt
log_Rt_next
log_Rt_next + log_Pt
exp(log_Rt_next + log_Pt)


# parameters
Pt <- vix[length(vix)][[1]] # initial price P_t
log_Pt <- log(Pt) # log of current prices log(P_{t})
log_Rt_next <- sim_rets_vix[, 1] # next set of returns log(R_{t+1})
Pt_next <- exp(log_Rt_next + log_Pt)

# display
Pt
log_Pt
log_Rt_next
log_Rt_next + log_Pt
exp(log_Rt_next + log_Pt)