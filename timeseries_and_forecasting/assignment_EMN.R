library("readxl")
library("dplyr")
library("urca")
library("MASS")
library("fGarch") 
### File location

data_path <-  "C:\\Users\\Guldan\\Desktop\\timeseriesanalysis\\data_assignment.xlsx"
  
### Read Data and Filter out the 2nd empty column
data <- read_excel(data_path, skip=1)



### Question 1 ###
#----------------#

### We will analyze the HFRI - Y8 time series
y8 <- data$EMN
y8 <- ts(y8, frequency=12, start = c(1990,4))
time_index <- data$date

### Identification Step

par(mfrow=c(1,2)) 
# Notes:
# 1) Mean of y8 = 0.0038 so almost no drift - offset.
# 2) Heteroscedacity between the signal samples.

plot(y8 ,type="l", col='blue', lwd=1, main="Time Series plot of EMN Monthly Returns", ylab="Monthly Returns", xlab="Years")
abline(h = mean(y8), col='red')
legend("topleft",legend=c("EMN values", "Mean of EMN"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

# It seems almost normal...
hist(y8, nclass=15, main="Histogram of EMN")

# Let's perform a Shapiro-Test for Normality.
shapiro.test(y8)

# For alpha level of .05 significance we fail to reject
# the Null Hypothesis, so the data are assumed to be normal
# for the rest fo the analysis.
par=(mfrow=c(1,1))
qqnorm(y8,main="Normal QQplot of EMN")      # normal Q-Q plot  
qqline(y8) # add a line    

par(mfrow=c(1,2)) # set up the graphics
acf(x=ts(y8,freq=1), lag.max = 60, main="ACF of EMN")   # Autocorrelation function plot
pacf(x=ts(y8,freq=1), lag.max = 60, main="PACF of EMN") # Partial Autocorrelation function


# We see that the significant terms are:
# MA - Terms 6:12:19
# AR - Terms 6:26:32

# Based on these plots EMA seems to be <<dumping>> shocks and 
# be almost stationary, lets see it with a test.


for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(y8,  lag=i, type="Ljung-Box")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," values"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(y8^2,lag=i, type="Ljung-Box") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," values"))
  }
}

# Let's Perform a Dickey Fuller Test
# On a small drift, but no-trend 
# On a lag-5 model. (Order of first 1st AR term minus 1)
m=ar(y8)
m1=ur.df(y8, type="drift",lags=m$order-1)
summary(m1)
### We reject the Null Hypothesis SO the Series is
### Assumed Stationary


### Estimation Step

# MA-1 model
ma1fit=arima(y8,order=c(0,0,1))
ma1fit
# Not significant coeff


# MA-6 model
ma6fit=arima(y8,order=c(0,0,6), fixed=c(0,0,0,0,0,NA,NA))
ma6fit
# Coeff is significant

# MA-6-12 model
ma6_12fit=arima(y8,order=c(0,0,12), fixed=c(0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
ma6_12fit
# Coeffs are significant


# AR-6 model
ar6fit=arima(y8,order=c(6,0,0),fixed=c(0,0,0,0,0,NA,NA))
ar6fit
# Coeff is significant

# AR-6-26 model
ar6_26fit=arima(y8,order=c(26,0,0),fixed=c(0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA), method="ML", optim.method="Nelder-Mead")
ar6_26fit
# Coeff are significant

arma66fit=arima(y8,order=c(6,0,6),fixed=c(0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
arma66fit
# Although ar term is significant / ma term is not

### Best Model: AR-6-26
ar6_26res=ar6_26fit$residuals


par(mfrow=c(3,2)) # set up the graphics

acf(ts(ar6_26res,freq=1), 60, main="ACF of residuals")

pacf(ts(ar6_26res,freq=1), 60, main="PACF of residuals")
# We see that the term AR-19 could be added...

acf(ts(ar6_26res^2,freq=1), 60, main="ACF of squared residuals")

pacf(ts(ar6_26res^2,freq=1), 60, main="PACF of squared residuals")


# Let's perform a Shapiro-Test for Normality on the Residuals.
shapiro.test(ar6_26res)
## We fail to reject the Null Hypothesis so the Residuals are COnsidered Normal

qqnorm(ar6_26res, main="Normal QQplot of residuals")
qqline(ar6_26res) 

Box.test(ar6_26res,lag=60,type="Ljung")
# In this test, however we fail to reject the null hypothesis, meaning 
# that there are correlations in the residuals

Box.test(ar6_26res^2,lag=60,type="Ljung")
# We reject the null hypothesis in square residuals, meaning 
# that there is no heteroscedasticuty problem in the squared residuals


##########################################
### Let's Add AR-19 term and try Again ###
##########################################

# AR-6-19-26 model
ar6_19_26fit=arima(y8,order=c(26,0,0),fixed=c(0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,NA,NA), method="ML", optim.method="Nelder-Mead")
ar6_19_26fit
# Coeff are significant, better aic
### Best Model: AR-6-19-26
ar6_19_26res=ar6_19_26fit$residuals


par(mfrow=c(3,2)) # set up the graphics

acf(ts(ar6_19_26res,freq=1), 60, main="ACF of residuals")

pacf(ts(ar6_19_26res,freq=1), 60, main="PACF of residuals")
# We see that the term AR-19 could be added...

acf(ts(ar6_19_26res^2,freq=1), 60, main="ACF of squared residuals")

pacf(ts(ar6_19_26res^2,freq=1), 60, main="PACF of squared residuals")


# Let's perform a Shapiro-Test for Normality on the Residuals.
shapiro.test(ar6_19_26res)
## We fail to reject the Null Hypothesis so the Residuals are COnsidered Normal

qqnorm(ar6_19_26res, main="Normal QQplot of residuals")
qqline(ar6_19_26res) 

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar6_19_26res,  lag=i, type="Ljung")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}
## No autocorrelation problem

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar6_19_26res^2,lag=i, type="Ljung") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}
## Heteroscedacity after the first 25 residuals

par(mfrow=c(1,1)) # set up the graphics


# Predictions
forecast=predict(ar6_19_26fit, 20)

UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y8,LL); maxx = max(y8,UL)

ts.plot(y8, forecast$pred, gpars=list(main="Time Series plot of EMN Monthly Returns", ylab="Monthly Returns", xlab="Years"))

lines(forecast$pred, col="red")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")



### Question 2 ###
#----------------#
# Rename Variables for easier handling #

data = data %>% rename(x1=`RUS-Rf`,
                x2=`RUS(-1)-Rf(-1)`,
                x3=`MXUS-Rf`,
                x4=`MEM-Rf`,
                x5=`SMB`,
                x6=`HML`,
                x7=`MOM`,
                x8=`SBGC-Rf`,
                x9=`SBWG-Rf`,
                x10=`LHY-Rf`,
                x11=`DEFSPR`,
                x12=`FRBI-Rf`,
                x13=`GSCI--Rf`,
                x14=`VIX`,
                x15=`Rf`) %>%  data.frame()

# Let's Fit a Simple LM

fit <- lm(y8~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15, data=data)

# Best AIC Model
model_AIC <- stepAIC(fit, direction="backward")

#Check Autocorrelation of the residuals and heteroscedasticity
AIC_res<-residuals(model_AIC)

par(mfrow=c(3,2))
shapiro.test(AIC_res)
# Residuals are not Normal
qqnorm(AIC_res,main="QQplot of Best AIC Model Residuals")  
qqline(AIC_res)

acf(AIC_res,lag.max=60, main="ACF of Best AIC Model Residuals")       # Minor Issues at lag 2:6:13:19
pacf(AIC_res,lag.max=60, main="PACF of Best AIC Model Residuals")     # Major Issues at lag 2:6:12
acf(AIC_res^2,lag.max=60, main="ACF of Best AIC Model Residuals^2")   # Minor Issues at lag 3:4:16
pacf(AIC_res^2,lag.max=60, main="PACF of Best AIC Model Residuals^2") # Major Issues at Lags 3:4

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(AIC_res,  lag=i, type="Ljung")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(AIC_res^2,lag=i, type="Ljung") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}



# Best BIC Model
model_BIC <- stepAIC(fit, direction = "backward", k =log(nrow(data)))

#Check Autocorrelation of the residuals and heteroscedasticity
BIC_res<-residuals(model_BIC)

par(mfrow=c(3,2))
shapiro.test(BIC_res)
# Residuals are not Normal
qqnorm(BIC_res,main="QQplot of Best BIC Model Residuals")  
qqline(BIC_res)

acf(BIC_res,lag.max=60, main="ACF of Best BIC Model Residuals")       # Minor Issues at lag 3:6:13:14
pacf(BIC_res,lag.max=60, main="PACF of Best BIC Model Residuals")     # Major Issues at lag 2:6:12
acf(BIC_res^2,lag.max=60, main="ACF of Best BIC Model Residuals^2")   # Minor Issues at lag 3:4:16
pacf(BIC_res^2,lag.max=60, main="PACF of Best BIC Model Residuals^2") # Major Issues at Lags 3:4

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(BIC_res,  lag=i, type="Ljung")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(BIC_res^2,lag=i, type="Ljung") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}



# Since the best BIC model is closer to rejecting normality and 
# Autocorrelation problems
# We will go with it


##################################################
### Fixing Autocorrelation in Regression Model ###
##################################################

### In order to fix the Autocorrelation
### Candidate MA Terms 2:6:13:14:19 
### Candidate AR Terms 2:6:13

ar2_6_6ma2_6=arima(BIC_res,order=c(6,0,6), fixed=c(0,NA,0,0,0,NA,0,NA,0,0,0,NA,NA))
ar2_6_6ma2_6res = ar2_6_6ma2_6$residuals

par(mfrow=c(3,2))
shapiro.test(ar2_6_6ma2_6res)
# Residuals are not Normal
qqnorm(ar2_6_6ma2_6res,main="QQplot of Best BIC Model Residuals")  
qqline(ar2_6_6ma2_6res)

acf(ar2_6_6ma2_6res,    lag.max=60, main="ACF of Best BIC Model Residuals")
pacf(ar2_6_6ma2_6res,   lag.max=60, main="PACF of Best BIC Model Residuals")
acf(ar2_6_6ma2_6res^2,  lag.max=60, main="ACF of Best BIC Model Residuals^2")  
pacf(ar2_6_6ma2_6res^2, lag.max=60, main="PACF of Best BIC Model Residuals^2") 

for(i in c(1,2,3,5,10,20,60)){
  result = Box.test(ar2_6_6ma2_6res,  lag=i, type="Ljung")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar2_6_6ma2_6res^2,lag=i, type="Ljung") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}

### There exists heteroscedasticity between the first 3-20 lags.
### Fix it with GARCH / ARCH modeling !
#######################################
# Estimate ARCH(3) model
arch_3=garchFit(~garch(3,0),data=ar2_6_6ma2_6res,trace=F) # trace = F reduces the summary
summary(arch_3) 



# Estimate GARCH(3,3) model
garch_3_3=garchFit(~garch(3,3),data=ar2_6_6ma2_6res,trace=F) # trace = F reduces the summary
summary(garch_3_3) 

plot(garch_3_3)


###########################################################
### Final Complete Model LM(x1,x5,x7,x8,x15) + ARMA([2,6],[2,6]) + GARCH(3,3) ###
##########################################################

###############
coef(model_BIC)
###############
regression_fix = garchSpec(model = list(
                      ar=c(0,-0.0321,0,0,0,0.1222), 
                      ma=c(0,-0.1257,0,0,0,0.0799),
                      mu=coef(garch_3_3)[[1]], 
                      omega=coef(garch_3_3)[[2]],
                      alpha=c(coef(garch_3_3)[[3]],coef(garch_3_3)[[4]],coef(garch_3_3)[[5]]),
                      beta=c(coef(garch_3_3)[[6]],coef(garch_3_3)[[7]],coef(garch_3_3)[[8]])
                      ), cond.dist = "norm")
####################
coef(regression_fix)
####################
