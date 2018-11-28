### EXERCISES WEEK 43 - SINGLE INDEX MODEL ###


### COLLECT THE DATA FROM THE LISTED STOCKS AND MARKET ###

library(quantmod)

tickers = c('AXP','MCD','GOOGL','XOM','IBM','NKE','WMT','KO','SPY')
getSymbols(tickers, from = '2008-1-1', to = '2018-1-1')
dates = index(AXP)


## Putting everything into data frame for easier calculations ##

AXP = as.data.frame(AXP$AXP.Adjusted)
MCD = as.data.frame(MCD$MCD.Adjusted)
GOOGL = as.data.frame(GOOGL$GOOGL.Adjusted)
XOM = as.data.frame(XOM$XOM.Adjusted)
IBM = as.data.frame(IBM$IBM.Adjusted)
NKE = as.data.frame(NKE$NKE.Adjusted)
WMT = as.data.frame(WMT$WMT.Adjusted)
KO = as.data.frame(KO$KO.Adjusted)
SPY = as.data.frame(SPY$SPY.Adjusted)

returnsCalc <- function(x){
  diff(x)/x[-length(x)]
}


## Compute the daily returns for each stock ## 
Returns = setNames(data.frame(matrix(ncol = 9, nrow = length(dates)-1), row.names = dates[-1]), tickers)

Returns$AXP = returnsCalc(AXP$AXP.Adjusted)
Returns$MCD = returnsCalc(MCD$MCD.Adjusted)
Returns$GOOGL = returnsCalc(GOOGL$GOOGL.Adjusted)
Returns$XOM = returnsCalc(XOM$XOM.Adjusted)
Returns$IBM = returnsCalc(IBM$IBM.Adjusted)
Returns$NKE = returnsCalc(NKE$NKE.Adjusted)
Returns$WMT = returnsCalc(WMT$WMT.Adjusted)
Returns$KO = returnsCalc(KO$KO.Adjusted)
Returns$SPY = returnsCalc(SPY$SPY.Adjusted)


## Compute and annualise the expected return and variance ##

geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

DailyAvg = apply(Returns, 2, geomAveCalc)
YearlyAvg = ((1+DailyAvg)^252) - 1  

DailyVar = apply(Returns, 2, var)
YearlyVar = DailyVar * 252


## Caclulate needed parameters for single index model ##

alpha = c()
beta = c()
mse = c()
for (i in 1:8){
  lmfit = lm(Returns[,i] ~ Returns$SPY)
  alpha[i] = lmfit$coefficients[1]
  beta[i] = lmfit$coefficients[2]
  mse[i] = mean(lmfit$residuals^2)
}

## With these estimates we will now compute the SIM mean and covariance ##

# Computing the annualised SIM average #
SIM_Avg = (1 + (alpha + beta * DailyAvg[length(DailyAvg)]))^252 - 1

# Computing the annualised covariance matrix from SIM #
SIM_Covar = matrix(ncol = 8, nrow = 8)

for (i in 1:8){
  for (j in 1:8){
    if (j == i){
      SIM_Covar[i,i] = beta[i]*beta[j]*DailyVar[length(DailyVar)] + mse[i]
    } else{
      SIM_Covar[i,j] = beta[i]*beta[j]*DailyVar[length(DailyVar)]  
    }
  }
}
SIM_Covar = SIM_Covar*252
SIM_Covar

real_Cov = cov(Returns[,1:8])*252



## Compute the efficient frontiers with the SIM mean and COV against the empirical ##


expPortRet = seq.int(0.02, 0.33, 0.01)


minVarPort = function(mu, Sigma, muP){
  N_assets = length(mu)
  
  KKT_mat = matrix(0,nrow = N_assets+2, ncol = N_assets+2)
  rhs = matrix(0, nrow = N_assets+2, ncol = 1)
  
  KKT_mat[1:N_assets,1:N_assets] = Sigma
  KKT_mat[1:N_assets, N_assets+1] = mu
  KKT_mat[1:N_assets, N_assets+2] = 1
  KKT_mat[N_assets+1, 1:N_assets] = mu
  KKT_mat[N_assets+2, 1:N_assets] = 1
  rhs[N_assets+1] = muP
  rhs[N_assets+2] = 1
  
  sol = solve(KKT_mat, rhs)
  weights = sol[1:N_assets]
  
  port_Return = t(mu) %*% as.matrix(weights)
  port_Risk = sqrt(t(weights) %*% Sigma %*% as.matrix(weights))
  
  return(list(port_Return, port_Risk))
}

minVarPort2 = function(mu, Sigma, muP){
  N_assets = length(mu)
  A = t(cbind(mu, matrix(1, N_assets, 1))) %*% solve(Sigma) %*% cbind(mu, matrix(1, N_assets, 1))
  weights = solve(Sigma) %*% cbind(mu, matrix(1, N_assets, 1)) %*% solve(A) %*% as.matrix(c(muP,1))
}

# Compute first with the Covariance defined with SIM #
count = 1
SIM_port_Return = c()
SIM_port_Risk = c()

for (muP in expPortRet){
  tmp = minVarPort(SIM_Avg, SIM_Covar, muP)
  
  SIM_port_Return[count] = muP
  SIM_port_Risk[count] = tmp[2]
  count = count + 1
}

# Compute now with empirical covariance and mean #

count = 1
emp_port_return = c()
emp_port_risk = c()

for (muP in expPortRet){
  tmp = minVarPort(YearlyAvg[1:8], real_Cov, muP)
  
  emp_port_return[count] = muP
  emp_port_risk[count] = tmp[2]
  count = count + 1
}


plot(SIM_port_Risk, SIM_port_Return, type = 'l', xlab = 'Risk', ylab = 'Portfolio Return', main = 'Efficient Frontiers computed from empirical and SIM parameters', cex.lab = 1.5)
lines(emp_port_risk, emp_port_return, type = 'l', col = 'red')
legend(0.14,0.3,c('SIM parameters', 'Empirical Parameters'),lwd = c(3.5,3.5,3.5), col=c('black','red'))
