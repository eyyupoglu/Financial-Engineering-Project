#Excercises week 38

#We start with extracting all the relevant information from the bonds from 
#http://www.nasdaqomxnordic.com/bonds/denmark
#We need the coupon, price and maturity date.

rm(list = ls())

#Getting lubridate
library(lubridate)

#1. Preparing the data----
#Prices extracted on 21.of september

Bond = c("4,5 St.l 39 GB",
        'Danske Stat 2025',
         'DGBI 2023 GB',
         '1,5St.l. 23GB',
         '3St.l 21GB',
         '4St.l.19 GB',
         '7 St.l 24GB',
         'DGBi',
         'Danske Stat 2018',
         'Danske Stat 2020',
         'Danske Stat 2027'
)
FaceValue = rep(100, length(Bond))
Price<-c(170.466,111.365,107.22,108.2,110.72,105.32,142.98,112.58,100.141,101.672,100.977)

Coupon<-c(4.5,1.75,0.1,1.5,3,4,7,0.1,0.25,0.25,0.5)
Matday<-c("2039-11-15","2025-11-15","2023-11-15","2023-11-15","2021-11-15","2019-11-15","2024-11-15","2030-11-15","2018-11-15","2020-11-15","2027-11-15")
Matyear<-c(2039,2025,2023,2023,2021,2019,2024,2030,2018,2020,2027)


#Now we combine all the relevant information to a dataframe
bondData <- data.frame(Bond, FaceValue, Coupon, Price, Matday)

#Now we get some more practical information
cashFlowDates <- c(today(), seq.Date(from= as.Date("2018-11-15"), to = as.Date("2039-11-15"), by = 'year'))
cashFlowDates

couponsToMaturity <- year(bondData$Matday)-year(today()) + 1 ## + 1 because we want to have the coupon payment for 2018 in the cashflows
couponsToMaturity
#We now calculate the dirty price and store it in our dataframe
bondData$dirtyPrice <- bondData$Price + (bondData$Coupon) * as.numeric(difftime(today(), "2017-11-15", units = 'days'))/365
bondData

#Part1a Setting up the cashflow for each bond----

# Start by creating an empty CashFlow Matrix which will then be filled
cashFlowsDirty <- matrix(0L, nrow = length(Bond), ncol = length(cashFlowDates))
#Assigning the rowvalues to the bond names
rownames(cashFlowsDirty) <- Bond
#Creating column names as the dates for the cashflows
colnames(cashFlowsDirty) <- as.character(cashFlowDates)
cashFlowsDirty

for (i in 1:length(Bond)){
  cashFlowsDirty[i,1] <- -bondData$dirtyPrice[i]   # Negative Cashflow when bond is bought
  for (j in 1:couponsToMaturity[i]){
    cashFlowsDirty[i,j+1] <- bondData$Coupon[i]   # Coupon payment
    if (j == couponsToMaturity[i]) {
      cashFlowsDirty[i,j+1] <- cashFlowsDirty[i,j+1] + bondData$FaceValue[i]  # Face value of bond paid at Maturity
    }
  }
}
cashFlowsDirty

#Part1b-Calculating the YTM for each bond----

#Function for the uniroot PV of a bond minus the dirty price

bval <- function(i, cf, t, dirtyP){
  
  sum(Payment/(1+i)^t)-dirtyP
  
}

rootsFun<-function(pay,time,dirty){
  
  output<-uniroot(bval,c(-1, 2),cf=pay, t=time ,dirtyP=dirty)$root #use the interval -2 to 2 meaning the ytm can take values bewteen -100% and 200%
  return(output)
}


YTM_dirty<-c()

for (j in 1:length(Matyear)){
  TimeToPayments<-c()
  Payment<-c()
  
  
  for (i in 2018:Matyear[j]){
    #print(i)
    TimeOfPayments<-seq( as.Date("2018-11-15"), by='year', len=Matyear[j]-2018+1)
    
    TimeToPayments[i-2018+1]<-as.numeric(difftime(TimeOfPayments[i-2018+1], today(), units='days'))/365
    
    Payment[i-2018+1]  <- Coupon[j]
  }
  
  Payment[i-2018+1]<-100+Payment[i-2018+1] #adding the last payment to the coupons
  YTM_dirty[j]<-rootsFun(Payment,TimeToPayments,bondData$dirtyPrice[j])
  
  
}

#Part1c-Plotting the bonds' yieldds for each maturity as a function of time to maturity----

#We can now add the YTM to our dataframe if we want
bondData$YTM=YTM_dirty
bondData
#Finally we make the plot and give the axis represantative names
plot(Matyear,bondData$YTM,xlab='Time',ylab='Yield to maturity')


#Week39----

#install.packages('YieldCurve')

library(YieldCurve)


# Calculating Years to maturities (using the total number of weeks until maturity)
bondData$maturities <- as.numeric(difftime(bondData$Matday, today(), unit="weeks"))/52.25
plot(bondData$maturities, bondData$YTM)

# Following is to get our data in same form as the one seen in example for NSrates and Nelson.Siegel see '?NSrates' (bottom)
bondData1=bondData[order(bondData$maturities),]

yields <- data.frame(as.list(bondData1$YTM))
rownames(yields) <- today()
yieldsXts <- as.xts(yields)
yieldsXts

# Compute the Nelson Siegel Parameters

#NSParameters <- Nelson.Siegel(bla, maturity=bondData1$maturities)
NSParameters <- Nelson.Siegel(yieldsXts, maturity=bondData1$maturities)
# Get the fitted Yields for each maturity using the Nelson Siegel parameters
fittedYield <- NSrates(NSParameters, bondData1$maturities)



# Plot the original Yield curve using the original data
plot(bondData1$maturities,yieldsXts[1,],main="Fitting Nelson-Siegel yield curve", type="o",xlab = 'Maturity in Years', ylab = 'Yield',cex.lab = 1.5)
# Add the Nelson.Siegel fitted values
lines(bondData1$maturities, fittedYield, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)


#Note that we have to remove one of the bonds, which one? Note that the DGBI bonds are index based bond
#while the others are nominal bonds, so we remove them


bondDataReduced<-bondData[-c(3,8),]

cashFlowsDirtyReduced <- cashFlowsDirty[-c(3,8),]


## Get data ready for Nelson Siegel
bondDataReduced <- bondDataReduced[order(bondDataReduced$maturities),]
yields <- data.frame(as.list(bondDataReduced$YTM))
rownames(yields) <- today()

yieldsXts <- as.xts(yields)
yieldsXts

NSParameters <- Nelson.Siegel(yieldsXts, maturity=bondDataReduced$maturities)
fittedYield <- NSrates(NSParameters, bondDataReduced$maturities)

plot(bondDataReduced$maturities,yieldsXts[1,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(bondDataReduced$maturities, fittedYield, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)

##### Part2 - Calculation of Duration and Convexity -----


# Function for Calculating Duration
calcDuration <- function(Price, CashFlow, t, r){
  1/Price * sum( t * cf / (1 + r)^(t+1))
}

# Function For Calculating Convexity
calcConvexity <- function(Price, CashFlow, t, r){
  1/Price * sum( t * (t+1) * cf / (1 + r)^(t+2))
}

dur <- c() # Empty array to store Duration Calculation
conv <- c() # Empty array to store Convexity Calculation

for (i in 1:length(bondData$Bond)){
  p <- bondData$dirtyPrice[i]
  cf <- cashFlowsDirty[i, ]
  t <- seq(along = cf)-1
  t[2:length(t)] <- t[2:length(t)] - ( 1 - as.numeric(difftime("2018-11-15", today(), units='days'))/365)  # here we account for the time until coupon payment
  
  r <- as.numeric(NSrates(NSParameters, t) / 100) # Use Nelson Sigel for estimation of rates
  r[1] <- 0                                 # First rate is NaN so we set it to Zero
  
  
  dur <- c(dur, calcDuration(p, cf, t, r))
  conv <- c(conv, calcConvexity(p, cf, t, r))
}

bondData$Duration <- dur
bondData$Convexity <- conv

bondData

