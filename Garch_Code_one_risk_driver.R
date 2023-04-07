sp500_rets = CalculateReturns(sp500, method = "log")
sp500_rets = sp500_rets[-1,]


mu_sp500_rets = mean(sp500_rets)
sd_sp500_rets = sd(sp500_rets)

# set.seed(1234)
# outcomes = matrix(NA,10000,5)
# dim(outcomes)
# 
# apply(outcomes, c(10000,5), rnorm, n = 1, mean = mu_sp500_rets, sd = sd_sp500_rets)

#part 3
library(rugarch)
spec = rugarch::ugarchspec(mean.model = list(armaOrder = c(1,1)),
                           variance.model = list(model = "sGARCH", 
                                                 garchOrder = c(1,1)),
                           distribution.model = "norm")

garchfit = rugarch::ugarchfit(data = sp500_rets, spec = spec)
garchforecast = ugarchforecast(fitORspec = garchfit, spec = spec, n.ahead = 5)

sp500_rets_forecast = matrix(NA,5,10000)

sigma(garchforecast)

set.seed(123)
for(i in 1:5)
{
  for(j in 1:10000)
  {
    sp500_rets_forecast[i,j] = rnorm(1, 
                                     mean = fitted(garchforecast)[i], 
                                     sd = sigma(garchforecast)[i])
  }
}






price = exp(sp500_rets_forecast[2,1]+log(sp500[length(sp500)]))

f_return_to_price <- function(p0, forecasted_rets)
{
  # p0: initial price
  # forecasted_rets: matrix of forecasted returns
  
  forecasted_prices = matrix(NA, nrow(forecasted_rets), ncol(forecasted_rets))
  temp = p0
  
  for(i in 1:nrow(forecasted_rets))
  {
    for(j in 1:ncol(forecasted_rets))
    {
      forecasted_prices[i,j] = exp(log(temp) + forecasted_rets[i,j])
    }
    temp = forecasted_prices[i,j]
  }
  return(forecasted_prices)
}

sp500_price_forecast = f_return_to_price(sp500[length(sp500)], sp500_rets_forecast)
sp500_price_forecast[1:5,1:100]


calls
#rugarch takes into account time correlation which takes into account more variance

#fitting model directly: in risk estimation, directly creating a forecast (works better to assume its gaussian) usefull when you repeat the same thing many times (10k in this case) better to perform risk analysis on this




#part 4
#term structure in options refers to the relationship between the expiration date of an option and its implied volatility. The rate of the term structure in options is also known as the volatility term structure or the volatility curve



