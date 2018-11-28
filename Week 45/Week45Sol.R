## WEEK 9 EXERCISES ##

# Clear variables #
rm(list = ls())

# Load packages #
library(quantmod)

# Get data #

getSymbols(c('MCD','KO','MSFT'), from = '1991-1-1', to = '2001-1-1', periodicity = 'weekly')

# Transform into data frames for easier data access and manipulaton #

MCD = as.data.frame(MCD)
KO = as.data.frame(KO)
MSFT = as.data.frame(MSFT)
dates = rownames(MCD)


## Compute the portfolio weights for the GMV and Tangent portfolio -- Assume risk free rate is 2% ##

## Compute weekly returns first and store into a data frame ##
returnCalc <- function(x){
  diff(x)/x[-length(x)]
}
tickers = c('MCD','KO','MSFT')

Returns = setNames(data.frame(matrix(ncol = length(tickers), nrow = length(dates)-1), row.names = dates[-1]), tickers)

Returns$MCD = returnCalc(MCD$MCD.Adjusted)
Returns$KO = returnCalc(KO$KO.Adjusted)
Returns$MSFT = returnCalc(MSFT$MSFT.Adjusted)


## Compute and annualise the expected return. Find the covariance matrix as well ##

geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

WeeklyAvg = apply(Returns, 2, geomAveCalc)
YearlyAvg = ((1+WeeklyAvg)^52) - 1  

covMat = cov(Returns) * 52  ## Annualised covariance matrix ##


## Define the A matrix from the calculation of the efficient frontier ##
l_mat = matrix(YearlyAvg,c(1,1,1), nrow = 2, ncol = 3)
l_mat[2,] = 1
r_mat = matrix(YearlyAvg, nrow = 3, ncol = 2)
r_mat[,2] = 1

A = l_mat %*% solve(covMat) %*% r_mat

a = A[1,1]
b = A[1,2]
c = A[2,2]

# Plot the efficient frontier

mu_P = seq(0.05, 0.4, 0.01)
sigma_P = seq(0,0.4,0.01)
plot( sqrt((c*mu_P^2 - 2*b*mu_P + a) / (a*c-b^2)), type = 'l', lwd = 2, mu_P, xlab = 'Risk',ylab = 'Expected return', ylim = c(0,0.4),xlim = c(0,0.5),main = 'CML and Efficient Frontier' ) 
points((sqrt(1/c)), b/c, pch = 'x', col = 'red', cex = 1.5)  

# Compute the GMV portfolio #
w_gmv = (1/c) * solve(covMat) %*% matrix(1,nrow = 3, ncol = 1)  # Find the portfolio weights of the GMV portoflio 
mu_gmv = t(YearlyAvg) %*% w_gmv 
sigma_gmv = t(w_gmv) %*% covMat %*% w_gmv

# Compute the CML (risk free rate is 2%) #

rf = 0.02
mu_e = YearlyAvg - rf 
#lines((mu_P-rf)/sqrt(t(YearlyAvg) %*% solve(covMat) %*% YearlyAvg), mu_P-rf, lwd = 2, col = 'blue')
lines(sigma_P, sigma_P*sqrt(t(mu_e)%*% solve(covMat)%*%mu_e) + rf, lwd = 2, col='blue')

# Compute tangent portfolio and plot it #

mu_tan = (t(mu_e) %*% solve(covMat) %*% mu_e) / (matrix(1,nrow=1, ncol= 3) %*% solve(covMat) %*% mu_e) 
sigma_tan = sqrt((t(mu_e) %*% solve(covMat) %*% mu_e)) / (matrix(1,nrow=1, ncol= 3) %*% solve(covMat) %*% mu_e)
points(sigma_tan, mu_tan+rf, pch = 'x', cex = 1.5, col = 'magenta')

# Find the weights of the tangent portfolio #

w_tan = (solve(covMat) %*% mu_e) %*% (mu_tan / (t(mu_e) %*% solve(covMat) %*% mu_e))


# Pick a point mu_C between the GMV and Tangent portfolio and find the portfolio. In this case we set mu_C = 0.25 or 25% #

mu_C = 0.25
w_C = solve(covMat) %*% r_mat %*% solve(A) %*% matrix(c(0.25,1),nrow=2,ncol=1)

C_portfolio_avg = t(YearlyAvg) %*% w_C
C_portfolio_risk = sqrt(t(w_C) %*% covMat %*% w_C)


## Find a fraction alpha that fulfills the described criterias ##

alpha = 1 - (mu_C - mu_gmv) / ((mu_tan+rf) - mu_gmv)

w_tmp = as.numeric(alpha) * as.matrix(w_gmv) + as.numeric((1-alpha)) * as.matrix(w_tan)

## Check if the 2 portfolios are identical ##
w_tmp - w_C

## Confirm that for each asset, the ratio of excess return to its' covariance with the tangent
## portfolio is identical. Hint: The covariance of asset with the tangent portfolio is easily
## obtained by multiplying portfolio weights with the covariance matrix.


mu_e / (t(w_tan) %*% covMat)


### SECOND PART OF THE EXERCISES ###

# Fit the linear model to compute the betas of the assets relative to the tangent portfolo
alpha = c()
beta = c()
mse = c()

for (i in 1:3){
  mdl = lm(Returns[,i] ~ as.matrix(Returns) %*% w_tan)
  alpha[i] = mdl$coefficients[1]
  beta[i] = mdl$coefficients[2]
  mse[i] = mean(mdl$residuals^2)
}

# Compute the beta of the tangent, gmv and 1(c) portfolio #

tan_port_beta = t(w_tan) %*% beta
gmv_port_beta = t(w_gmv) %*% beta
C_port_beta   = t(w_C)   %*% beta


# Put it in a beta/return plot #

plot(c(tan_port_beta, gmv_port_beta, C_port_beta),c(mu_tan+rf, mu_gmv, mu_C), lwd = 3, type='l', xlim = c(0.5,1.5), ylim = c(0.0,0.43), main='Market Security Line', xlab = 'Beta',ylab='Return')
points(c(tan_port_beta, gmv_port_beta, C_port_beta),c(mu_tan+rf, mu_gmv, mu_C), col = 'red', cex = 2, pch = 'x', lwd = 2)
points(beta,YearlyAvg, col = 'blue', cex = 2, lwd = 2)

#dataLabels = c('Tan.Port.', 'GMV Port.', 'mu_C Port.','MCD','KO','MSFT')
#text(c(tan_port_beta, gmv_port_beta, C_port_beta, beta), c(mu_tan+rf, mu_gmv, mu_C, YearlyAvg), labels=dataLabels, cex= 1.5, pos = 1)

## Seems like the three portfolios lie on the market security line ##

## Plot historical average returns as a function of beta from th data of week 43, 1 and the market data as well S&P500 ##

Tickers = c('AXP','MCD', 'GOOGL', 'XOM', 'IBM','NKE','WMT','KO','SPY')
getSymbols(Tickers, from = '2008-1-1', to = '2018-1-1', periodicity = 'daily')


# Put everything into data frame #
AXP = as.data.frame(AXP$AXP.Adjusted)
MCD = as.data.frame(MCD$MCD.Adjusted)
GOOGL = as.data.frame(GOOGL$GOOGL.Adjusted)
XOM = as.data.frame(XOM$XOM.Adjusted)
IBM = as.data.frame(IBM$IBM.Adjusted)
NKE = as.data.frame(NKE$NKE.Adjusted)
WMT = as.data.frame(WMT$WMT.Adjusted)
KO = as.data.frame(KO$KO.Adjusted)
SPY = as.data.frame(SPY$SPY.Adjusted)
dates_capm = rownames(SPY)


# Set up the return data frame #
ReturnsMat = setNames(data.frame(matrix(ncol = 9, nrow = length(dates_capm)- 1), row.names = dates_capm[-1]), nm = Tickers)


ReturnsMat$AXP = returnCalc(AXP$AXP.Adjusted)
ReturnsMat$MCD = returnCalc(MCD$MCD.Adjusted)
ReturnsMat$GOOGL = returnCalc(GOOGL$GOOGL.Adjusted)
ReturnsMat$XOM = returnCalc(XOM$XOM.Adjusted)
ReturnsMat$IBM = returnCalc(IBM$IBM.Adjusted)
ReturnsMat$NKE = returnCalc(NKE$NKE.Adjusted)
ReturnsMat$WMT = returnCalc(WMT$WMT.Adjusted)
ReturnsMat$KO = returnCalc(KO$KO.Adjusted)
ReturnsMat$SPY = returnCalc(SPY$SPY.Adjusted)

# Compute the betas of the stocks # 
alpha_CAPM = c()
beta_CAPM = c()
mse_CAPM = c()
for (i in 1:8){
  lmfit = lm(ReturnsMat[,i] ~ ReturnsMat[,9])
  alpha_CAPM[i] = lmfit$coefficients[1]
  beta_CAPM[i] = lmfit$coefficients[2]
  mse_CAPM[i] = mean(lmfit$residuals^2)
}


# Annualise the returns #
annual_Returns = (1+apply(ReturnsMat, 2, geomAveCalc))^252 - 1


# plot the points #
points(beta_CAPM, annual_Returns[1:8], pch = 'o', col = 'green', lwd = 2, cex = 1.3)





