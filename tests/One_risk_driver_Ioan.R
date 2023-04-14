source(here("code", "Utils.R"))
source(here("code", "OptionPricing.R"))


#Calculating returns of the SP500
sp500_rets = PerformanceAnalytics::CalculateReturns(sp500, method = "log")
sp500_rets = sp500_rets[-1,]



mean_sp500 = mean(sp500_rets) #mean of sp500
sd_sp500 = sd(sp500_rets) #standard deviation of sp500



#initialize matrix of returns forecasted until T+5
sp500_rets_forecast = matrix(NA,10000,5)

#simulating 10k returns using the mean and the std dev from our sp500 returns
for(n_ahead in 1:5)
{
  sp500_rets_forecast[1:10000, n_ahead] = rnorm(10000,
                                                mean = mean_sp500,
                                                sd = sd_sp500)
}


#initialize matrix of prices forecasted until T+5
sp500_price_forecast = matrix(NA,10000,5)


p_prev = sp500[length(sp500)][[1]] #previous price (before T+1)

#converting returns to prices
for(i in 1:5)
{
  sp500_price_forecast[,i] = f_next_Pt(p_prev,
                                       sp500_rets_forecast[,i])
  p0 = sp500_rets_forecast[,i]
}
colnames(sp500_price_forecast) =  c("T+1", "T+2", "T+3", "T+4", "T+5")



T = c(20,20,40,40)  #Time to maturity of our book of options

K = c(1600,1650,1750,1800)  #Strike price of our book of options

#Matrix of call prices initialisation
call_price_matrices <- lapply(rep(1, 4), # generate three empty matrices of compatible sizes
                              function(x){
                                matrix(NA, 
                                       nrow(sp500_price_forecast), 
                                       ncol(sp500_price_forecast),
                                       dimnames=list(seq(1:nrow(sp500_price_forecast)),
                                                     c("T+1", "T+2", "T+3", "T+4", "T+5")
                                       )
                                )
                              }
)

#matrices of P&L initialization
PL_matrices = call_price_matrices


#Loop to calculate the P&L of each option from our book of options
for(i in 1:4)
{
  for(j in 1:5)
  {
    price_call = prc_opt(T[i]-j,K[i], calls, rf_mat, sp500_price_forecast[,j], vix[[length(vix)]])
    call_price_matrix[[i]][,j] = price_call
    PL_matrices[[i]][,j] = option_profit(S = sp500_price_forecast[,j], K = K[i], c =  price_call)$call_profit
  }
}




opt1_VaR_ES = f_VaR_ES(PL_matrices[[1]], alpha = 0.05)  #VaR95 and ES95 option 1 of our book of options
opt2_VaR_ES = f_VaR_ES(PL_matrices[[2]], alpha = 0.05)  #VaR95 and ES95 option 2 of our book of options
opt3_VaR_ES = f_VaR_ES(PL_matrices[[3]], alpha = 0.05)  #VaR95 and ES95 option 3 of our book of options
opt4_VaR_ES = f_VaR_ES(PL_matrices[[4]], alpha = 0.05)  #VaR95 and ES95 option 4 of our book of options

opt1_VaR_ES
opt2_VaR_ES
opt3_VaR_ES
opt4_VaR_ES




