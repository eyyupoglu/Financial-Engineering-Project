#Exercises for Week 2

# Exercise 1(a-b) ----
# Clearing the workspace
rm(list = ls())


# Load the  necessary packages 
library(quantmod)
library(lubridate)
# Load previous weeks data
getSymbols('XOM',src = 'yahoo', from = "1995-02-20",periodicity = "weekly")

# Define it as a data frame for easier access to variables 
XOM = as.data.frame(XOM)

# Save the dates in a seperate variable 
dates = as.Date(rownames(XOM))

# Compute weekly returns and log returns 

# Function to calculate returns
ComputeReturn <- function(x){
  diff(x)/x[-length(x)]
}

WeeklyRet = ComputeReturn(XOM$XOM.Adjusted)


logReturns = log(XOM$XOM.Adjusted[-1]) - log(XOM$XOM.Adjusted[1:length(XOM$XOM.Adjusted)-1])



# Compute the average of the weekly returns and log-returns 

geoMean = function(returns){
  avg = prod(1+returns)^(1/length(returns)) - 1
}

geoAvg = geoMean(WeeklyRet)
geoAvg

AverageLogReturns = mean(logReturns)
AverageLogReturns


# Annualize the weekly returns 
annual_returns = (1+geoAvg)^52 - 1
annual_returns

annual_log_returns = AverageLogReturns*52
annual_log_returns

# 1C: Difference between log annual returns and "normal" returns---- 

annual_returns - annual_log_returns


# Exercise 2a----

# Load data directly with getSymbols, note that this will extract updated data from the day you issue the command
#unless you type in a "to" input


getSymbols(c('SPY','XLF','EEM'), src = 'yahoo', from = '2004-12-31',to='2018-09-01')

# Store in data frames 
SPY = as.data.frame(SPY)
XLF = as.data.frame(XLF)
EEM = as.data.frame(EEM)
# To analyse big data you can make a quick sanity check by looking at the first and last rows with the following
?head
?tail
head(SPY)
tail(SPY)

# Store the new dates in a variable 
dates2 = as.Date(rownames(SPY))

# Compute returns for each ETF 
SPY_Returns = ComputeReturn(SPY$SPY.Adjusted)
XLF_Returns = ComputeReturn(XLF$XLF.Adjusted)
EEM_Returns = ComputeReturn(EEM$EEM.Adjusted)

# Average the daily returns
AverageReturn_SPY = geoMean(SPY_Returns)
AverageReturn_XLF = geoMean(XLF_Returns)
AverageReturn_EEM = geoMean(EEM_Returns)

# Annualise the daily returns 
SPY_annual = (1+AverageReturn_SPY)^252 - 1
XLF_annual = (1+AverageReturn_XLF)^252 - 1
EEM_annual = (1+AverageReturn_EEM)^252 - 1

# Exercise 2b----
# Annualise the standard deviations 

sd_SPY = sd(SPY_Returns)*sqrt(252)
sd_XLF = sd(XLF_Returns)*sqrt(252)
sd_EEM = sd(EEM_Returns)*sqrt(252)

# Exercise 2c----
# Annualise the covariance matrix 
# Collect all data in a matrix
dailyReturns = cbind(SPY_Returns, XLF_Returns, EEM_Returns)

Var_Cov_Matrix = cov(dailyReturns) * 252

# Compute correlation matrix 
cor_mat = cor(dailyReturns)
cor_mat

# 2.d explanation----
# For the correlation matrix we dont need to do anything since correlation is a normalized quantity 
# and is therefore not affected by the scale of the variables

# 2.e Calendar Year----
# Store the historical adjusted prices in a single data.frame
# Store the historical adjusted prices in a single data.frame
PriceDaily <- data.frame(SPY$SPY.Adjusted,  XLF$XLF.Adjusted,EEM$EEM.Adjusted, row.names = as.Date(rownames(SPY)))
head(PriceDaily)
tail(PriceDaily)

# Calculate returns
returnsDaily <- apply(PriceDaily, 2, ComputeReturn)
head(returnsDaily)


# Calculate daily Geometric mean for each year 
?aggregate 
?year

# Calculate  avg return and standard deviation for each year
yearGeomAve <- aggregate(returnsDaily, by=list(year = year(as.Date(rownames(returnsDaily)))), geoMean)
yearGeomAve
yearStd<-aggregate(returnsDaily,by=list(year = year(as.Date(rownames(returnsDaily)))), sd)
yearStd

# Annualize the  returns 
AnnYearGeomAve <- data.frame(year = yearGeomAve$year, (1+yearGeomAve[,-1])^252-1)
AnnYearGeomAve

# Annulize the standard deviation
Ann_STD_SPY=yearStd$SPY*sqrt(252)
Ann_STD_XLF=yearStd$XLF*sqrt(252)
Ann_STD_EEM=yearStd$EEM*sqrt(252)

AnnSTD <- data.frame( year = yearStd$year, SPY=Ann_STD_SPY,XLF=Ann_STD_XLF,EEM=Ann_STD_EEM)
AnnSTD

# 2F Use daily data to extract the final price off all months----
# Extract Monthly Data
SPYmonthly <- as.data.frame(to.monthly(SPY, indexAt = 'endof'))
XLFmonthly <- as.data.frame(to.monthly(XLF, indexAt = 'endof'))
EEMmonthly <- as.data.frame(to.monthly(EEM, indexAt = 'endof'))

#Getting just the adjusted price
SPYmonthly_adj=SPYmonthly$SPY.Adjusted
XLFmonthly_adj=XLFmonthly$XLF.Adjusted
EEMmonthly_adj=EEMmonthly$EEM.Adjusted

PriceMonthly <- data.frame(SPY = SPYmonthly$SPY.Adjusted, XLF = XLFmonthly$XLF.Adjusted, EEM = EEMmonthly$EEM.Adjusted, row.names = as.Date(rownames(SPYmonthly)))
head(PriceMonthly)

#Calculate Monthly returns all in once
returnsMonthly <- apply(PriceMonthly, 2, ComputeReturn)
returnsMonthly
head(returnsMonthly)

#Calculating the monhly returns for each ETF
SPY_Monthly_Returns = ComputeReturn(PriceMonthly$SPY)
XLF_Monthly_Returns = ComputeReturn(PriceMonthly$XLF)
EEM_Monthly_Returns = ComputeReturn(PriceMonthly$EEM)


#Average the monthly returns
AverageMonthlyReturn_SPY = geoMean(SPY_Monthly_Returns)
AverageMonthlyReturn_XLF = geoMean(XLF_Monthly_Returns)
AverageMonthlyReturn_EEM = geoMean(EEM_Monthly_Returns)

#Annualise the monthly returns
SPY_annual_Monthly = (1+AverageMonthlyReturn_SPY)^12 - 1
XLF_annual_Monthly = (1+AverageMonthlyReturn_XLF)^12 - 1
EEM_annual_Monthly = (1+AverageMonthlyReturn_EEM)^12 - 1

#Annualise the standard deviations
SPY_Annual_Monthly_sd = sd(SPY_Monthly_Returns)*sqrt(12)
XLF_Annual_Monthly_sd = sd(XLF_Monthly_Returns)*sqrt(12)
EEM_Annual_Monthly_sd = sd(EEM_Monthly_Returns)*sqrt(12)
