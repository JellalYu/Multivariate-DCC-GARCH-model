library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(rmgarch)
options(digits=4)

computer = "work"
if (computer == "home") {
  setwd("C:\\Users\\ezivot\\Dropbox\\econ589\\R\\")
}
if (computer == "work") {
  setwd("C:\\Users\\ezivot.SOCIOLOGY\\Dropbox\\econ589\\R\\")
}

source("covEWMA.r")

# download data
symbol.vec = c("MSFT", "^GSPC")
getSymbols(symbol.vec, from ="2000-01-03", to = "2012-04-03")
colnames(MSFT)
start(MSFT)
end(MSFT)

# extract adjusted closing prices
MSFT = MSFT[, "MSFT.Adjusted", drop=F]
GSPC = GSPC[, "GSPC.Adjusted", drop=F]

# plot prices
plot(MSFT)
plot(GSPC)

# calculate log-returns for GARCH analysis
MSFT.ret = CalculateReturns(MSFT, method="log")
GSPC.ret = CalculateReturns(GSPC, method="log")


# remove first NA observation
MSFT.ret = MSFT.ret[-1,]
GSPC.ret = GSPC.ret[-1,]
colnames(MSFT.ret) ="MSFT"
colnames(GSPC.ret) = "GSPC"

# create combined data series
MSFT.GSPC.ret = merge(MSFT.ret,GSPC.ret)

# plot returns
plot(MSFT.ret)
plot(GSPC.ret)

# scatterplot of returns
plot( coredata(GSPC.ret), coredata(MSFT.ret), xlab="GSPC", ylab="MSFT",
      type="p", pch=16, lwd=2, col="blue")
abline(h=0,v=0)

#
# compute rolling correlations
#
# chart.RollingCorrelation(MSFT.ret, GSPC.ret, width=20)

cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=20,
                     by.column=FALSE, align="right") #zoo ???象可以接受不??????的??????序列???据
roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=100,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="20-day rolling covariances", ylab="covariance", lwd=2, col="blue")
grid() #可以畫虛線方格
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")#畫一般的相關係數
plot(roll.cor, main="100-day rolling correlations",ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

#
# calculate EWMA covariances and correlations
#
lambda <- 0.94
cov.ewma <- covEWMA(as.data.frame(MSFT.GSPC.ret), lambda=lambda)

## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1];
### conditional correlation
t <- length(cov.ewma[,1,1]);
MSFT.GSPC.cond.cor<- rep(0,t);
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2];
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500");
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500");
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))

# compute rolling covariances and correlations using longer window
roll.cov = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=252,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=252,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="252-day rolling covariances",
     ylab="covariance", lwd=2, col="blue")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="252-day rolling correlations",
     ylab="correlation", lwd=2, col="blue")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))



# compute EWMA covariances and correlations using longer half-life
half.life = 125 
lambda = exp(log(0.5)/half.life)
cov.ewma <- covEWMA(MSFT.GSPC.ret, lambda=lambda)


## 2. extract conditional variance and correlation
### conditional variance
MSFT.GSPC.cond.cov <- cov.ewma[,2,1]
### conditional correlation
t <- length(cov.ewma[,1,1])
MSFT.GSPC.cond.cor<- rep(0,t)
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i]<- cov2cor(cov.ewma[i,,])[1,2]
}
### Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,
     type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",
     main="EWMA Covariance between MSFT and S&P500")
grid()
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,
     type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",
     main="EWMA Correlation between MSFT and S&P500")
grid()
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


#
# DCC estimation
#

# univariate normal GARCH(1,1) for each series
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")
dcc.garch11.spec

dcc.fit = dccfit(dcc.garch11.spec, data = MSFT.GSPC.ret)
class(dcc.fit)
summary(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, 
# residuals, plot, infocriteria, rcor, rcov
# show, nisurface

# show dcc fit
dcc.fit

# plot method
plot(dcc.fit)
# Make a plot selection (or 0 to exit): 
#   
# 1:   Conditional Mean (vs Realized Returns)
# 2:   Conditional Sigma (vs Realized Absolute Returns)
# 3:   Conditional Covariance
# 4:   Conditional Correlation
# 5:   EW Portfolio Plot with conditional density VaR limits

# conditional sd of each series
plot(dcc.fit, which=2)

# conditional correlation
plot(dcc.fit, which=4)

# extracting correlation series
ts.plot(rcor(dcc.fit)[1,2,])

#
# forecasting conditional volatility and correlations
#

dcc.fcst = dccforecast(dcc.fit, n.ahead=100)
class(dcc.fcst)
slotNames(dcc.fcst)
class(dcc.fcst@mforecast)
names(dcc.fcst@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# show forecasts
dcc.fcst

# plot forecasts
4