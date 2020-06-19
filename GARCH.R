##### Log-returns analysis from wig20 index

#### 1. Data preprocessing

data <- read.csv("wig20_d.csv")   ## source: stooq

head(data) 

xt <- data[,5] # vector of close price
rt <- 100*diff(log(xt))  ## log-returns multiply by 100 in %

plot(rt,type="h",col="blue",main="log-returns from wig20 [%] in XII 2019 - VI 2020")

length(xt)

### Visualization 

par(mfrow=c(2,1)) #to have 2 plots in one

plot(xt,type="l",col="blue",lwd=2,xlab="number of session",ylab="WIG20",main="Daily index quotations WIG20 in XII 2019 - VI 2020")
abline(h=c(1400,1600,1800,2000,2200),col="gainsboro",lty=3)
abline(v=55,col="gray",lty=3)

plot(rt,type="h",col="blue",lwd=2,xlab="number of session",ylab="r(t)",main="Daily log-returns [%] from WIG20 in XII 2019 - VI 2020",las=1)
abline(h=c(-10,-5,0,5),col="gainsboro",lty=3)
abline(v=55,col="gray",lty=3) 

### Until coronovirus became to Poland index WIG20 was quite calm,
### after when its happened we can observe large fluctuations.
### That deny the classic random Gaussian sample.

### Now let's look on this index in two defferent periods.

### Division on two time series (XII 2019 - II 2020; III 2020 - VI 2020) and another observations

rt1 <- rt[1:55]
rt2 <- rt[55:121]

par(mfrow=c(2,2))

plot(rt1,type="h",col="purple",lwd=2,xlab="number of session",ylab="r(t) [%]",main="Log-returns in XII 2019 - II 2020",las=1)
abline(h=c(-4,-3,-2,-1,0,1,2),col="gainsboro",lty=3)

plot(55:121,rt2,type="h",col="purple",lwd=2,xlab="number of session",ylab="r(t) [%]",main="Log-returns in III 2020 - VI 2020",las=1)
abline(h=c(-10,-5,0,5),col="gainsboro",lty=3)

# As we can see difference is huge, only need to look on r(t) scale on both plots

# Assumption of independence and Gaussian log-returns of stocks is a key postulate 
# for dynamics describing the process of {St} price for certain instrument.

# We can diagnoze it by qqplot or normality tests.

## QQ-plot

qqnorm(rt1,pch=16,col="blue",xlab="gaussian quantiles",ylab="empirical quantiles",main="QQ-plot for r(t) - XII 2019 - II 2020",las=1)
qqline(rt1,col="red")

qqnorm(rt2,pch=16,col="purple",xlab="gaussian quantiles",ylab="empirical quantiles",main="QQ-plot for r(t) - III 2020 - VI 2020")
qqline(rt2,col="red")

## Descriptive statistics
library(quantmod)
library(tseries)
library(e1071)

summary(rt)
summary(rt1)
summary(rt2)

sd(rt)
sd(rt1)
sd(rt2)

skewness(rt)
skewness(rt1)
skewness(rt2)

kurtosis(rt)  
kurtosis(rt1)
kurtosis(rt2)



### Statistical tests for normality (Lillieforce'a, Shapiro-Wilka, Jarque-Bera).

library(nortest)
library(fGarch)

lillie.test(rt1)
lillie.test(rt2)

shapiro.test(rt1)
shapiro.test(rt2)

jarque.bera.test(rt1)
jarque.bera.test(rt2)

## Shapiro and jarque-bera test definitely reject normality of sample (p-value is very small),
## lillie test weakly reject normality, it's depend of significance level.

### Generalized Error Distribution Parameter Estimation

rt1ged <- gedFit(rt1) #Function to fit the parameters of the generalized error distribution.
rt1ged$par

rt2ged <- gedFit(rt2) #Function to fit the parameters of the generalized error distribution.
rt2ged$par

# nu - shape parameter

### Autocorrelation; nonlinear effects type (G)ARCH

aacfrt1 <- acf(rt1,lag.max=20,col="blue",lwd=2,type="correlation",xlab="h",main="ACF(h) for {r(t)}",las=1)
aacfrt2 <- acf(rt2,lag.max=20,col="blue",lwd=2,type="correlation",xlab="h",main="ACF(h) for {r(t)}",las=1)

aacfrt1sq <- acf((rt1)^2,lag.max=20,col="blue",lwd=2,type="correlation",xlab="h",main="ACF(h) dla {r(t)^2} (2019)",las=1)
aacfrt2sq <- acf((rt2)^2,lag.max=20,col="blue",lwd=2,type="correlation",xlab="h",main="ACF(h) dla {r(t)^2} (2020)",las=1)

## Dotted lines set area for " white noise",
## it's mean when value of ACF is higher than this area, then it's a white noise


## Take ACF(h) for h>0 and better visualizations ACF without ACF(0)=1 for better scaling
ACFrt1 <- aacfrt1$acf[-1]
ACFrt2 <- aacfrt2$acf[-1]

ACFrt1sq <- aacfrt1sq$acf[-1]
ACFrt2sq <- aacfrt2sq$acf[-1]


par(mfrow=c(2,2))

plot(ACFrt1,type="h",col="blue",lwd=2,xlab="h",ylab="ACF",main="ACF for log-returns (I period)",las=1)
abline(h=0,col="gray55",lty=1)
abline(h=c(-1.96/sqrt(83),1.96/sqrt(83)),col="red",lty=2)

plot(ACFrt2,type="h",col="blue",lwd=2,xlab="h",ylab="ACF",main="ACF fo log-returns(II period)",las=1)
abline(h=0,col="gray55",lty=1)
abline(h=c(-1.96/sqrt(83),1.96/sqrt(83)),col="red",lty=2)

plot(ACFrt1sq,type="h",col="blue",lwd=2,xlab="h",ylab="ACF",main="ACF for squared log-returns (I period)",las=1)
abline(h=0,col="gray55",lty=1)
abline(h=c(-1.96/sqrt(83),1.96/sqrt(83)),col="red",lty=2)

plot(ACFrt2sq,type="h",col="blue",lwd=2,xlab="h",ylab="ACF",main="ACF for squared log-returns (II period)",las=1)
abline(h=0,col="gray55",lty=1)
abline(h=c(-1.96/sqrt(83),1.96/sqrt(83)),col="red",lty=2)

## As we can see, it's look "white noisy".

### Let's use tests Ljunga-Boxa and Engle'a for check total significance of correlation coefficients

Box.test(rt1,lag=10,type = "Ljung-Box",fitdf=0)

Box.test(rt1^2,lag=10,type = "Ljung-Box",fitdf=0)

Box.test(rt2,lag=10,type = "Ljung-Box",fitdf=0)

Box.test(rt2^2,lag=10,type = "Ljung-Box",fitdf=0)

#For each tests p-value is large, so we can say that the residuals are independent

# End of the first part


library(fGarch)

# Simulation trajectories of the example Garch model with fGarch library

n_ <- 150

garch<- garchSpec(model = list(alpha = 0.25, beta = 0.48, omega = 1e-4),cond.dist = "norm")  ## gaussian noise

gedgarch <- garchSpec(model = list(alpha = 0.25, beta = 0.48, omega = 1e-4,shape = 1.2),cond.dist = "ged")  ## GED(alfa=1,3)


## b) symulacja trajektorii

garchrt <- garchSim(spec = garch, n = n_, n.start = 150, extended = FALSE)  ## trajectory with white noise

garchrtgvec <- garchSim(spec = garch, n = n_, n.start = 150, extended = TRUE) 


gedgarchrtn <- garchSim(spec = gedgarch, n = n_, n.start = 150, extended = FALSE)  ## trajektoria szeregu z szumem GED

gedgarchrtngvec <- garchSim(spec = gedgarch, n = n_, n.start = 150, extended = TRUE)  ## szereg, sigma_t i innowacje z szumem GED


# c) First visualization

par(mar = c(5,5,6,5))  

plot(1:n_,garchrt,type="l",col="blue",xlab="t",ylab="GARCH(1,1) gaussian")

par(new=T)

plot(gedgarchrtn,type="l",col="red",xlab=NULL,ylab=NULL,axes=FALSE,las=1)

axis(side = 4)

mtext(side = 4, line = 2, 'GARCH(1,1) not-gaussian (GED)')

mtext(side = 3, line = 2, "Trajectories of GARCH(1,1) model with lenght 200")


# d) Second visualization: Garch points: {r(t)}, volatility {sigma(t)}, noise {epsilon(t)}

par(mfrow=c(3,1))

plot(1:n_,gedgarchrtngvec$garch,type="h",col="blue",lwd=2,xlab="t",ylab="r(t)",main="Trajectories of GARCH model",las=1)
abline(h=0,col="gray",lty=3)

plot(1:n_,gedgarchrtngvec$sigma,type="l",col="blue",lwd=2,xlab="t",ylab="sigma(t)",main="GARCH Volatility",las=1)

plot(1:n_,gedgarchrtngvec$eps,pch=16,col="blue",lwd=2,xlab="t",ylab="epsilon(t)",main="GARCH Noise ",las=1)
abline(h=0,col="gray",lty=3)


# 2. Quasi-Maximum Likelihood Estimation of the parameters

# Estimates the parameters of an univariate ARMA-GARCH/APARCH process.
garches <- garchFit(formula = ~ garch(1,1),data = garchrt,cond.dist = "norm") 

# a) Results analysis

garches@fit$llh  ## -log likelihood for model  (our goal is to minimaize it)

garches@fit$coef   ## QMLE estimators for this model

# coefficients are quite similar to alpha = 0.25, beta = 0.48, omega = 1e-4

garches@fit$se.coef  ## SE(\theta_QML) - standard errors

garches@fit$tval  ## T statistics for significance analysis

garches@fit$cvar ## covariance-variance matrix

## b) Visualizations model volatility and returns 

sigmat_ <- garches@sigma.t  # sigma(t) 

plot(1:n_,as.numeric(sigmat_),type="l",xlab="t",ylab="sigma(t)",main="volatility")


epst_ <- garches@residuals  # epsilon(t) 

plot(1:n_,as.numeric(epst_),xlab="t",ylab="epsilon(t)",main="GARCH returns")

