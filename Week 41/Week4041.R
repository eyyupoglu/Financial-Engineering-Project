
setwd("") #Set the working directory

#Week 40&41 42104 Introduction to Financial Engineering

#################PART1#################

# Read data for McDonalds, Cooca Cola and Microsoft from file 
MCD <- read.csv("MCD.csv", header = TRUE, sep = ',', row.names = 1)
KO <- read.csv("KO.csv", header = TRUE, sep = ',', row.names = 1)
MSFT <- read.csv("MSFT.csv", header = TRUE, sep = ',', row.names = 1)

# Store Adjusted Close Prices for stocks in a single data frame
dates <- rownames(MCD)
Prices <- data.frame(MCD=MCD$Adj.Close, KO=KO$Adj.Close, MSFT=MSFT$Adj.Close, row.names = dates)
head(Prices)

# Function for calculating continous returns & log returns
returnsCalc <- function(x){
  diff(x)/x[-length(x)]
}

Returns <- apply(Prices, 2, returnsCalc)
head(Returns)

# Function for calculating geometric mean
geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

weeklymean <- apply(Returns, 2, geomAveCalc)

yearlymean <- matrix((1+weeklymean)^52-1)
#yearlystd <-  sqrt (52 * apply(Returns, 2, var))
yearlycov <- 52 * cov(Returns)

# Now think about a portfolio  p is such that in total p_1 + p_2 + p_3 = 1 and we look at the following
# combinations of the form (0.1, 0.1, 0.8), (0.1, 0.2, 0.7) and so forth

# Create a matrix of portfolio weights
totNumbPort <- 11+10+9+8+7+6+5+4+3+2+1 # Total number of portfolios

portfolios <- matrix(, nrow = totNumbPort, ncol = 3)
port_mean <- c()
port_var <- c()
port_std <- c()

count <- 1

for (i in 0:10){
  for (j in 0:(10-i)){
    k <-  10 - i - j
    #So if you think about your portfolio weights as x
    x = c(i, j, k) / 10
    portfolios[count, ] <- x 
    port_mean[count] <- t(x) %*% yearlymean 
    port_var[count] <- t(x) %*% yearlycov %*% x
    port_std[count] <- sqrt(port_var[count])
    count <- count+1
  }
}

portfolios
port_mean
port_var
port_std

# Plot portfolios
plot(port_std, port_mean, xlab = 'standard deviation', ylab = 'mean', main = 'Portfolio Performance')

# Highest expected return
max_r <-  match(max(port_mean),port_mean) # Find portfolio with highest return
portfolios[max_r, ]

# Plot highest return on plot
points(port_std[max_r], port_mean[max_r], col = 'red', pch=8, cex=3)

# Lowest standard deviation
min_std <-  match(min(port_std),port_std) # Find portfolio with the lowest standard deviation
portfolios[min_std, ]

# Plot lowest standard deviation
points(port_std[min_std], port_mean[min_std], col = 'blue', pch=8, cex=3)

# Highest slope (Sharpe Ratio)
port_sharpe = port_mean / port_std

max_slope = match(max(port_sharpe), port_sharpe)
portfolios[max_slope, ]

# Plot max Sharpe Ratio on plotdeviation
points(port_std[max_slope], port_mean[max_slope], col = 'green', pch=8, cex=3)


#################PART2#################


# Function for computing 
minVarPort <- function(R, mu, sigma, Rp){
  # This function finds the portfolio with the minimum variance given an Expected Portfolio Return Level
  # this function can easily be much more general 
  
  # Here we calculate covariance matrix Sigma from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu1 <- matrix(c(mu,rep(1, length(mu))), ncol=2)
  
  A <-t(mu1) %*% solve(Sigma) %*% mu1
  
  Rp1 <- matrix(c(Rp, 1), ncol = 1)
  
  w <- solve(Sigma) %*% mu1 %*% solve(A) %*% Rp1
  
  min_var_mu <- A[1,2] / A[2,2]
  min_var_sigma <- sqrt( 1/ A[2,2])
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, min_var_mu = min_var_mu, min_var_sigma = min_var_sigma))
}

minVarPortRf <- function(R, mu, sigma, Rp, Rf){
  # This function finds the portfolio with the minimum variance given 
  # an Expected Portfolio Return Level and a Risk-Free Rate
  # this function can easily be much more general 
  # e.g. mu, RF, sigma can be parameters
  
  # Here we calculate covariance matrix S from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu_e <- mu-Rf
  
  w_temp1 <- solve(Sigma) %*% mu_e
  
  w_temp2 <- (Rp-Rf) / t(mu_e) %*% solve(Sigma) %*% mu_e
  
  w <-w_temp1 %*% w_temp2
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu + (1-sum(w)) * Rf
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  opt2_sigma <- sqrt( (Rp)^ 2 / t(mu_e) %*% solve(Sigma) %*% mu_e)
  
  tangent_mu = t(mu_e) %*% solve(Sigma) %*% mu_e / t(matrix(c(1,1), nrow=2)) %*% solve(Sigma) %*% mu_e + Rf
  tangent_sigma = sqrt( t(mu_e) %*% solve(Sigma) %*% mu_e) / t(matrix(c(1,1), nrow=2)) %*% solve(Sigma) %*% mu_e 
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, tangent_mu = tangent_mu, tangent_sigma = tangent_sigma))
}

# These are constant throughout the excercise
Rf <- .02
sigma <- matrix(c(.1, .2), nrow=2, ncol=1) 
mu <- matrix(c(.1, .2), nrow = 2, ncol = 1)

#five different correlation matrix
R <- list()
R[[1]] <- matrix(c(1,.5,.5, 1), nrow = 2, ncol = 2)
R[[2]] <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
R[[3]] <- matrix(c(1,-.5,-.5 ,1), nrow = 2, ncol = 2)
R[[4]] <- matrix(c(1,.999999,.999999,1), nrow = 2, ncol = 2)
R[[5]] <- matrix(c(1,-.999999,-.999999,1), nrow = 2, ncol = 2)

# Set the required portfolio expected return
Rp = mu[1]

#### WITHOUT risk-free asset shorting allowed #### 
p1 <- minVarPort(R[[1]], mu, sigma, Rp)
p2 <- minVarPort(R[[2]], mu, sigma, Rp) 
p3 <- minVarPort(R[[3]], mu, sigma, Rp) 
p4 <- minVarPort(R[[4]], mu, sigma, Rp)
p5 <- minVarPort(R[[5]], mu, sigma, Rp)

RpStep <- seq(0,1.5*max(mu), length.out = 100) # Different Expected Portfolio Requirments

OptStore <- list()
for (i in 1:length(R)){
  port_mu <- c()
  port_sigma <- c()
  for (j in RpStep){
    temp <- minVarPort(R[[i]], mu, sigma, j)
    port_mu <- c(port_mu, temp$port_mu)
    port_sigma <- c(port_sigma, temp$port_sigma)
  }
  
  OptStore[[i]] <- list(port_mu=port_mu,port_sigma=port_sigma, min_var_sigma = temp$min_var_sigma, min_var_mu = temp$min_var_mu)
}

plot(OptStore[[1]]$port_sigma,OptStore[[1]]$port_mu, type = 'l', xlim = c(0,2*max(mu)), ylim= c(0, 2*max(sigma)),
     xlab = 'std', ylab = 'return', main = 'Efficient Frontier')
lines(OptStore[[2]]$port_sigma,OptStore[[2]]$port_mu, type = 'l', col = 'red')
lines(OptStore[[3]]$port_sigma,OptStore[[3]]$port_mu, type = 'l', col = 'blue')
lines(OptStore[[4]]$port_sigma,OptStore[[4]]$port_mu, type = 'l', col = 'green')
lines(OptStore[[5]]$port_sigma,OptStore[[5]]$port_mu, type = 'l', col = 'purple')

# Add Minimum variance Portfolio
points(OptStore[[1]]$min_var_sigma,OptStore[[1]]$min_var_mu, pch = 3)
points(OptStore[[2]]$min_var_sigma,OptStore[[2]]$min_var_mu, col = 'red', pch = 3)
points(OptStore[[3]]$min_var_sigma,OptStore[[3]]$min_var_mu, col = 'blue',  pch = 3)
points(OptStore[[4]]$min_var_sigma,OptStore[[4]]$min_var_mu, col = 'green',  pch = 3)
points(OptStore[[5]]$min_var_sigma,OptStore[[5]]$min_var_mu, col = 'purple',  pch = 3)


#### With Risk free asset included #####
# Calculate Minimum Variance Portfolio given Rp
p1rf <- minVarPortRf(R[[1]], mu, sigma, Rp, Rf)
p2rf <- minVarPortRf(R[[2]], mu, sigma, Rp, Rf)
p3rf <- minVarPortRf(R[[3]], mu, sigma, Rp, Rf)
p4rf <- minVarPortRf(R[[4]], mu, sigma, Rp, Rf)
p5rf <- minVarPortRf(R[[5]], mu, sigma, Rp, Rf)

RpStep <- seq(Rf,1.5*max(mu), length.out = 100) # Different Expected Portfolio Requirments

OptStoreRF <- list()
for (i in 1:length(R)){
  port_mu <- c()
  port_sigma <- c()
  for (j in RpStep){
    temp <- minVarPortRf(R[[i]], mu, sigma, j, Rf)
    port_mu <- c(port_mu, temp$port_mu)
    port_sigma <- c(port_sigma, temp$port_sigma)
  }
  OptStoreRF[[i]] <- list(port_mu=port_mu,port_sigma=port_sigma, tangent_sigma = temp$tangent_sigma, tangent_mu = temp$tangent_mu)
}

lines(OptStoreRF[[1]]$port_sigma,OptStoreRF[[1]]$port_mu)
lines(OptStoreRF[[2]]$port_sigma,OptStoreRF[[2]]$port_mu, col = 'red')
lines(OptStoreRF[[3]]$port_sigma,OptStoreRF[[3]]$port_mu, col = 'blue')
lines(OptStoreRF[[4]]$port_sigma,OptStoreRF[[4]]$port_mu, col = 'green')
lines(OptStoreRF[[5]]$port_sigma,OptStoreRF[[5]]$port_mu, col = 'purple')


# Add Tangent portfolios
points(OptStoreRF[[1]]$tangent_sigma, OptStoreRF[[1]]$tangent_mu)
points(OptStoreRF[[2]]$tangent_sigma, OptStoreRF[[2]]$tangent_mu, col = 'red')
points(OptStoreRF[[3]]$tangent_sigma, OptStoreRF[[3]]$tangent_mu, col = 'blue')
points(OptStoreRF[[4]]$tangent_sigma, OptStoreRF[[4]]$tangent_mu, col = 'green')
points(OptStoreRF[[5]]$tangent_sigma, OptStoreRF[[5]]$tangent_mu, col = 'purple')


#################PART3#################
# Read data for McDonalds, Cooca Cola and Microsoft from file 
MCD <- read.csv("MCD.csv", header = TRUE, sep = ',', row.names = 1)
KO <- read.csv("KO.csv", header = TRUE, sep = ',', row.names = 1)
MSFT <- read.csv("MSFT.csv", header = TRUE, sep = ',', row.names = 1)

# Store Adjusged Close Prices for stocks in a single data frame
dates <- rownames(MCD)
Prices <- data.frame(MCD=MCD$Adj.Close, KO=KO$Adj.Close, MSFT=MSFT$Adj.Close, row.names = dates)
head(Prices)

# Function for calculating continous returns

returnsCalc <- function(x){
  diff(x)/x[-length(x)]
}

Returns <- apply(Prices, 2, returnsCalc)
head(Returns)

# Function for calculating geometric mean
geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

weeklymean <- apply(Returns, 2, geomAveCalc)


# Calculate parameters needed for efficient frontier computations.
yearlymean <- matrix((1+weeklymean)^52-1)
yearlycov <- 52 * cov(Returns)
CorMat <- cor(Returns)
Rf = .02

# Plot Each As
plot(sqrt(diag(yearlycov)), yearlymean, ylim  = c(0,1.2*max(yearlymean)), xlim = c(0, 1.2*max(sqrt(yearlycov))))


## WITHOUT risk free asset
# Using only two assets 
mu = as.matrix(yearlymean[-3], ncol = 1)
sigma = matrix(sqrt(diag(yearlycov[-3,-3])), ncol = 1)
R = CorMat[-3,-3]

# Function for computing 
minVarPort <- function(R, mu, sigma, Rp){
  # This function finds the portfolio with the minimum variance given an Expected Portfolio Return Level
  # this function can easily be much more general 
  
  # Here we calculate covariance matrix Sigma from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu1 <- matrix(c(mu,rep(1, length(mu))), ncol=2)
  
  A <-t(mu1) %*% solve(Sigma) %*% mu1
  
  Rp1 <- matrix(c(Rp, 1), ncol = 1)
  
  w <- solve(Sigma) %*% mu1 %*% solve(A) %*% Rp1
  
  min_var_mu <- A[1,2] / A[2,2]
  min_var_sigma <- sqrt( 1/ A[2,2])
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, min_var_mu = min_var_mu, min_var_sigma = min_var_sigma))
}

minVarPortRf <- function(R, mu, sigma, Rp, Rf){
  # This function finds the portfolio with the minimum variance given 
  # an Expected Portfolio Return Level and a Risk-Free Rate
  # this function can easily be much more general 
  # e.g. mu, RF, sigma can be parameters
  
  # Here we calculate covariance matrix S from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu_e <- mu-Rf
  
  w_temp1 <- solve(Sigma) %*% mu_e
  
  w_temp2 <- (Rp-Rf) / t(mu_e) %*% solve(Sigma) %*% mu_e
  
  w <-w_temp1 %*% w_temp2
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu + (1-sum(w)) * Rf
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  opt2_sigma <- sqrt( (Rp)^ 2 / t(mu_e) %*% solve(Sigma) %*% mu_e)
  
  tangent_mu = t(mu_e) %*% solve(Sigma) %*% mu_e / t(matrix(rep(1, length(mu_e)), nrow=length(mu_e))) %*% solve(Sigma) %*% mu_e + Rf
  tangent_sigma = sqrt( t(mu_e) %*% solve(Sigma) %*% mu_e) / t(matrix(rep(1,length(mu_e)), nrow=length(mu_e))) %*% solve(Sigma) %*% mu_e 
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, tangent_mu = tangent_mu, tangent_sigma = tangent_sigma))
}

RpStep <- seq(0,1.5*max(mu), length.out = 1000) # Different Expected Portfolio Requirments

port_mu <- c()
port_sigma <- c()
port_mu_rf <- c()
port_sigma_rf <- c()
for (j in RpStep){
  temp <- minVarPort(R, mu, sigma, j)
  if (j >= Rf) {
    temprf <- minVarPortRf(R, mu, sigma, j, Rf)
    port_mu_rf <- c(port_mu_rf, temprf$port_mu)
    port_sigma_rf <- c(port_sigma_rf, temprf$port_sigma)
  }
  temprf <- minVarPortRf(R, mu, sigma, j, Rf)
  port_mu <- c(port_mu, temp$port_mu)
  port_sigma <- c(port_sigma, temp$port_sigma)
}

lines(port_sigma, port_mu)
#Min variance Portfolio
points(temp$min_var_sigma, temp$min_var_mu, pch = 3)

# With risky asset
lines(port_sigma_rf, port_mu_rf)
#Tangent Portfolio
points(temprf$tangent_sigma, temprf$tangent_mu, pch = 4, col = 'blue')


# Add Microsoft
mu = yearlymean
sigma = matrix(sqrt(diag(yearlycov)), ncol = 1)
R = CorMat

RpStep <- seq(0,1.5*max(mu), length.out = 1000) # Different Expected Portfolio Requirments

port_mu <- c()
port_sigma <- c()
port_mu_rf <- c()
port_sigma_rf <- c()
for (j in RpStep){
  temp <- minVarPort(R, mu, sigma, j)
  if (j >= Rf) {
    temprf <- minVarPortRf(R, mu, sigma, j, Rf)
    port_mu_rf <- c(port_mu_rf, temprf$port_mu)
    port_sigma_rf <- c(port_sigma_rf, temprf$port_sigma)
  }
  temprf <- minVarPortRf(R, mu, sigma, j, Rf)
  port_mu <- c(port_mu, temp$port_mu)
  port_sigma <- c(port_sigma, temp$port_sigma)
}

lines(port_sigma, port_mu, col = 'red')
#Min variance Portfolio
points(temp$min_var_sigma, temp$min_var_mu, pch = 4, col= 'red')

# With risky asset
lines(port_sigma_rf, port_mu_rf, col = 'red')
# Tangent Portfolio
points(temprf$tangent_sigma, temprf$tangent_mu, pch = 4, col = 'red')

#################PART4#################


# Read data for McDonalds, Cooca Cola and Microsoft from file 
MCD <- read.csv("MCD.csv", header = TRUE, sep = ',', row.names = 1)
KO <- read.csv("KO.csv", header = TRUE, sep = ',', row.names = 1)
MSFT <- read.csv("MSFT.csv", header = TRUE, sep = ',', row.names = 1)

# Store Adjusged Close Prices for stocks in a single data frame
dates <- rownames(MCD)
Prices <- data.frame(MCD=MCD$Adj.Close, KO=KO$Adj.Close, MSFT=MSFT$Adj.Close, row.names = dates)
head(Prices)

# Function for calculating continous returns

returnsCalc <- function(x){
  diff(x)/x[-length(x)]
}

Returns <- apply(Prices, 2, returnsCalc)
head(Returns)

# Function for calculating geometric mean
geomAveCalc <- function(x){
  (prod((1+x)))^(1/length(x))-1
}

weeklymean <- apply(Returns, 2, geomAveCalc)


# Calculate parameters needed for efficient frontier computations.
yearlymean <- matrix((1+weeklymean)^52-1)
yearlycov <- 52 * cov(Returns)
CorMat <- cor(Returns)
Rf = .02

# Plot Each As
plot(sqrt(diag(yearlycov)), yearlymean, ylim  = c(0,1.2*max(yearlymean)), xlim = c(0, 1.2*max(sqrt(yearlycov))))


## WITHOUT risk free asset
# Using only two assets 
mu = as.matrix(yearlymean[-3], ncol = 1)
sigma = matrix(sqrt(diag(yearlycov[-3,-3])), ncol = 1)
R = CorMat[-3,-3]

# Function for computing 
minVarPort <- function(R, mu, sigma, Rp){
  # This function finds the portfolio with the minimum variance given an Expected Portfolio Return Level
  # this function can easily be much more general 
  
  # Here we calculate covariance matrix Sigma from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu1 <- matrix(c(mu,rep(1, length(mu))), ncol=2)
  
  A <-t(mu1) %*% solve(Sigma) %*% mu1
  
  Rp1 <- matrix(c(Rp, 1), ncol = 1)
  
  w <- solve(Sigma) %*% mu1 %*% solve(A) %*% Rp1
  
  min_var_mu <- A[1,2] / A[2,2]
  min_var_sigma <- sqrt( 1/ A[2,2])
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, min_var_mu = min_var_mu, min_var_sigma = min_var_sigma))
}

minVarPortRf <- function(R, mu, sigma, Rp, Rf){
  # This function finds the portfolio with the minimum variance given 
  # an Expected Portfolio Return Level and a Risk-Free Rate
  # this function can easily be much more general 
  # e.g. mu, RF, sigma can be parameters
  
  # Here we calculate covariance matrix S from using correlation and std.  
  Sigma <- diag(c(sigma)) %*% R %*% diag(c(sigma))
  
  mu_e <- mu-Rf
  
  w_temp1 <- solve(Sigma) %*% mu_e
  
  w_temp2 <- (Rp-Rf) / t(mu_e) %*% solve(Sigma) %*% mu_e
  
  w <-w_temp1 %*% w_temp2
  
  # Return value
  port <- w
  opt_mu <- t(w) %*% mu + (1-sum(w)) * Rf
  opt_sigma <- sqrt( t(w) %*% Sigma %*% w)
  
  opt2_sigma <- sqrt( (Rp)^ 2 / t(mu_e) %*% solve(Sigma) %*% mu_e)
  
  tangent_mu = t(mu_e) %*% solve(Sigma) %*% mu_e / t(matrix(rep(1, length(mu_e)), nrow=length(mu_e))) %*% solve(Sigma) %*% mu_e + Rf
  tangent_sigma = sqrt( t(mu_e) %*% solve(Sigma) %*% mu_e) / t(matrix(rep(1,length(mu_e)), nrow=length(mu_e))) %*% solve(Sigma) %*% mu_e 
  return(list(port_weigths = port, port_mu = opt_mu, port_sigma = opt_sigma, tangent_mu = tangent_mu, tangent_sigma = tangent_sigma))
}

RpStep <- seq(0,1.5*max(mu), length.out = 1000) # Different Expected Portfolio Requirments

port_mu <- c()
port_sigma <- c()
port_mu_rf <- c()
port_sigma_rf <- c()
for (j in RpStep){
  temp <- minVarPort(R, mu, sigma, j)
  if (j >= Rf) {
    temprf <- minVarPortRf(R, mu, sigma, j, Rf)
    port_mu_rf <- c(port_mu_rf, temprf$port_mu)
    port_sigma_rf <- c(port_sigma_rf, temprf$port_sigma)
  }
  temprf <- minVarPortRf(R, mu, sigma, j, Rf)
  port_mu <- c(port_mu, temp$port_mu)
  port_sigma <- c(port_sigma, temp$port_sigma)
}

lines(port_sigma, port_mu)
#Min variance Portfolio
points(temp$min_var_sigma, temp$min_var_mu, pch = 3)

# With risky asset
lines(port_sigma_rf, port_mu_rf)
#Tangent Portfolio
points(temprf$tangent_sigma, temprf$tangent_mu, pch = 4, col = 'blue')


# Add Microsoft
mu = yearlymean
sigma = matrix(sqrt(diag(yearlycov)), ncol = 1)
R = CorMat

RpStep <- seq(0,1.5*max(mu), length.out = 1000) # Different Expected Portfolio Requirments

port_mu <- c()
port_sigma <- c()
port_mu_rf <- c()
port_sigma_rf <- c()
for (j in RpStep){
  temp <- minVarPort(R, mu, sigma, j)
  if (j >= Rf) {
    temprf <- minVarPortRf(R, mu, sigma, j, Rf)
    port_mu_rf <- c(port_mu_rf, temprf$port_mu)
    port_sigma_rf <- c(port_sigma_rf, temprf$port_sigma)
  }
  temprf <- minVarPortRf(R, mu, sigma, j, Rf)
  port_mu <- c(port_mu, temp$port_mu)
  port_sigma <- c(port_sigma, temp$port_sigma)
}

lines(port_sigma, port_mu, col = 'red')
#Min variance Portfolio
points(temp$min_var_sigma, temp$min_var_mu, pch = 4, col= 'red')

# With risky asset
lines(port_sigma_rf, port_mu_rf, col = 'red')
# Tangent Portfolio
points(temprf$tangent_sigma, temprf$tangent_mu, pch = 4, col = 'red')


############################################################
################## PART 4 ##############################
############################################################

# Problem  # 1 - No Riskless lending, and no shorting
#
# \min_w w' * Sigma * w
# st.  
#  w'1 = 1     or                    w[1]+w[2]+w[3] = 1
# w'mu = Rp    or  mu[1]*w[1]+mu[2]*w[2]+mu[3]*w[3] = Rp
# w_i >= 0     or                   w[1],w[2],w[3] >= 0

#install.packages('Rsolnp')
library('Rsolnp')

# Objective Function
fn1=function(w, Sigma, mu)
{
  Sigma[1,1]*w[1]^2 + Sigma[2,2]*w[2]^2 + Sigma[3,3]*w[3]^2 +
    2*w[1]*w[2]*Sigma[1,2] + 
    2*w[1]*w[3]*Sigma[1,3] + 
    2*w[2]*w[3]*Sigma[2,3]
}

# Equality Constraints (Left hand side)
eq_left1=function(w, Sigma, mu){
  z1=w[1] + w[2] + w[3]
  z2=mu[1]*w[1] + mu[2]*w[2] + mu[3]*w[3]
  return(c(z1,z2))
}

# Right hand Side of Equality constraints
eq_right1 = c(1, Rp)

# Inequality constratins as lowerbounds of elements
lb = c(0, 0, 0)

#Initial guess
x0 = c(.1,.1,.8)

# Initialize parameters
Sigma = yearlycov
Rp = 0.2

eq_right1 = c(1, Rp)

ObjOptim=solnp(x0, fun = fn1, eqfun = eq_left1, eqB = eq_right1, LB = lb, 
               Sigma = Sigma, mu = mu)


# Problem  # 2 - No - shorting (w[4] amount ivested in iriskfree lending)
# \min_w w*Sigma*w
# st.
#  w'1 = 1     or                    w[1]+w[2]+w[3]+w[4] = 1
# w'mu = Rp or     mu[1]*w[1] + mu[2]*w[2] + mu[3]*w[3] * w[4]*Rf = Rp
# w_i >= 0  or                                 w[1], w[2], w[3] >= 0

# Objective Function
fn2=function(w, Sigma, mu, Rf)
{
  Sigma[1,1]* w[1]^2 + Sigma[2,2]* w[2]^2 + Sigma[3,3] * w[3]^2 +
    2*w[1]*w[2]*Sigma[1,2] + 
    2*w[1]*w[3]*Sigma[1,3] + 
    2*w[2]*w[3]*Sigma[2,3]
}

# Equality Constraints (Left hand side)
eq_left2=function(w, Sigma, mu, Rf){
  z1 = w[1]+ w[2] + w[3] + w[4]
  z2 = mu[1]*w[1] + mu[2]*w[2] + mu[3]*w[3] + w[4]*Rf
  return(c(z1,z2))
}

# Right hand Side of Equality constraints
# eq_right2 = c(1, Rp)

# Inequality constratins as lowerbounds of elements
lb = c(0, 0, 0, 0)

# Initialize parameters
Rf = 0.02
Rp = .2

eq_right2 = c(1, Rp)

#Initial guess
x0 = c(.1,.1,.7,.1)

ObjOptim=solnp(pars=x0, fun = fn2, eqfun = eq_left2, eqB = eq_right2, LB = lb, 
               Sigma = Sigma, mu = mu, Rf = Rf)


RpStep <- seq(Rf*1.0001, max(mu)*0.9999, length.out = 1000) # Different Expected Portfolio Requirments

port_mu <- c()
port_sigma <- c()
port_mu_rf <- c()
port_sigma_rf <- c()

for (j in RpStep){
  eq_right2 = c(1,j)
  temprf <- solnp(pars = c(.1,.1,.7, .1), fun = fn2, eqfun = eq_left2, eqB = eq_right2, LB = c(0, 0, 0, 0), 
                  Sigma = Sigma, mu = mu, Rf = Rf)
  
  port_mu_rf <- c(port_mu_rf, temprf$pars[1:3] %*% mu + temprf$pars[4]*Rf)
  port_sigma_rf <- c(port_sigma_rf, temprf$values[length(temprf$values)])
  
  if (j >= min(mu)) {
    eq_right1 = c(1, j)
    temp <- solnp(pars = c(.1,.1,.8), fun = fn1, eqfun = eq_left1, eqB = eq_right1, LB = c(0, 0, 0), 
                  Sigma = Sigma, mu = mu)
    port_mu <- c(port_mu, temp$pars %*% mu)
    port_sigma <- c(port_sigma, temp$values[length(temp$values)])
  }
}

lines(sqrt(port_sigma_rf), port_mu_rf, col = 'green')

lines(sqrt(port_sigma), port_mu, col = 'black')

