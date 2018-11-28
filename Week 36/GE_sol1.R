## Group Exercises for Week 1 ##

## Load data through quantmod ##
install.packages("quantmod")

## Load the installed package ##
library(quantmod)

## Use getSymbols to directly get access to finance data ##

getSymbols('XOM',src = 'yahoo', from = "1995-02-20",periodicity = "weekly")

## Define it as a data frame for easier access to variables ##
XOM = as.data.frame(XOM)

## Save the dates in a seperate variable ##
dates = as.Date(rownames(XOM))


## Plot both the closing and adjusted closing price
plot(dates,XOM$XOM.Close, type = 'l', xlab = 'Dates', ylab = 'Price', main = 'XOM Closing and Adjusted Closing Price', cex.lab = 1.5, col = "black")
lines(dates,XOM$XOM.Adjusted, col = "red",lwd = 1, type = 'l')
legend("topleft",legend = c('Close','Adj. Close'), cex = 1.2, lty = c(1,1), col = c('black','red') )

## Calculate weekly returns ##

ComputeReturn = function(Prices){
  returns = diff(Prices) / Prices[-1]
}

WeeklyRet = ComputeReturn(XOM$XOM.Adjusted)


## Compute the geometric average of the weekly returns ##

geoMean = function(returns){
  avg = prod(1+returns)^(1/length(returns)) - 1
}

geoAvg = geoMean(WeeklyRet)
geoAvg

## Compute the standard deviation of the returns ## 

sd(WeeklyRet)

## Compute log returns and show difference between "real" returns ## 

logReturns = log(XOM$XOM.Adjusted[-1]) - log(XOM$XOM.Adjusted[1:length(XOM$XOM.Adjusted)-1])

AverageLogReturns = mean(logReturns)
AverageLogReturns

plot(dates[-1], logReturns - WeeklyRet, type = 'l', xlab = 'Dates',ylab = 'Log-return', main = 'Log-returns',cex.lab = 1.5)



### DATA AND RETURNS ###


## Load data directly with getSymbols ##

getSymbols(c('SPY','XLF','EEM'), src = 'yahoo', from = '2005-01-01')

## Store in data frames ##
SPY = as.data.frame(SPY)
XLF = as.data.frame(XLF)
EEM = as.data.frame(EEM)

## Store the new dates in a variable
dates2 = as.Date(rownames(SPY))

## Plot the adjusted closing prices on same graph ##
plot(dates2,SPY$SPY.Adjusted, type = 'l', xlab = 'Dates', ylab = 'Adj. Close', cex.lab = 1.5, col = 'black',main = 'Adj. Closing Prices for SPY, EEM and XLF', ylim = c(0,300))
lines(dates2, XLF$XLF.Adjusted, type = 'l', col = 'red')
lines(dates2, EEM$EEM.Adjusted, type = 'l', col = 'blue')
legend("topleft", legend = c('SPY','XLF','EEM'), col = c('black','red','blue'), lty = 1, lw = 2)


## Compute daily returns, average daily returns and standard deviations for each ETF ##

#Returns
SPY_Returns = ComputeReturn(SPY$SPY.Adjusted)
XLF_Returns = ComputeReturn(XLF$XLF.Adjusted)
EEM_Returns = ComputeReturn(EEM$EEM.Adjusted)

#Daily average with geometric mean
AverageReturn_SPY = geoMean(SPY_Returns)
AverageReturn_XLF = geoMean(XLF_Returns)
AverageReturn_EEM = geoMean(EEM_Returns)

#Standard deviation
sd_SPY = sd(SPY_Returns)
sd_XLF = sd(XLF_Returns)
sd_EEM = sd(EEM_Returns)

## Compute variance-covariance matrix ##

#Collect all data in a matrix
dailyReturns = cbind(SPY_Returns, XLF_Returns, EEM_Returns)

Var_Cov_Matrix = cov(dailyReturns)

## Compute correlation matrix ##
cor_mat = cor(dailyReturns)
cor_mat



