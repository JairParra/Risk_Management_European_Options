
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

###################################################################



### Volatility Surface


$$
  \begin{aligned}
\overset{\rightarrow}{\alpha}^{*} &=  \underset{\overset{\rightarrow}{\alpha}}{\arg \min} \;\; \sum_{t=1}^{T} \left|\sigma_{t}^{observed} - \sigma(m .\tau)\right| \\ 
&= \underset{\alpha_1, \alpha_2, \alpha_3, \alpha_4}{\arg \min} \;\; \sum_{t=1}^{T} \left|\sigma_{t}^{observed} - 
  \left( \alpha_1 + \alpha_2(m-1)^{2} + \alpha_3(m-1)^{3} + \alpha_4 \sqrt{\tau}  \right)
\right|
  \end{aligned}
$$
  
  $$
  \sigma(m,\tau) = \alpha_1 + \alpha_2(m-1)^{2} + \alpha_3(m-1)^{3} + \alpha_4 \sqrt{\tau}
$$
  
  $$
  m = \dfrac{S}{K} 
$$
  
  ```{r}
# obtain last price 
S <- sp500[length(sp500)][[1]]

# convert to draframe for easier manipulation 
calls_df <- as.data.frame(calls)
puts_df <- as.data.frame(puts)

# assign extra column to puts (1) and calls (0)
calls_df["type"] <- "call"
puts_df["type"] <- "put"

# check dimensions 
dim(calls_df) 
dim(puts_df)

# stack both of these matrices together 
puts_calls <- rbind(calls_df, puts_df) 

# intergate the price 
puts_calls["S"] <- S
puts_calls["m"] <- puts_calls["K"]/puts_calls["S"]

# filter the calls that have moniness over one 
calls_m_over <- puts_calls[(puts_calls["type"] == "call") & (puts_calls["m"] >= 1), ]

# filter the puts that have moniness below one
puts_m_under <- puts_calls[(puts_calls["type"] == "put") & (puts_calls["m"] < 1), ]

# combine these results into putcalls again 
call_put_data <- rbind(calls_m_over, puts_m_under)
head(call_put_data)
```