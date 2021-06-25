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

### We will analyze the HFRI - y1 time series
y1 <- data$HFRI
y1 <- ts(y1, frequency=12, start = c(1990,4))
time_index <- data$date

### Identification Step

par(mfrow=c(1,2)) 
# Notes:
# 1) Mean of y1 = 0.0078 so almost no drift - offset.
# 2) Heteroscedacity between the signal samples.

plot(y1 ,type="l", col='blue', lwd=1, main="Time Series plot of HFRI Monthly Returns", ylab="Monthly Returns", xlab="Years")
abline(h = mean(y1), col='red')
legend("topleft",legend=c("HFRI values", "Mean of HFRI"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

# It seems almost normal with 2 outliers
hist(y1, nclass=15, main="Histogram of HFRI")

# Let's perform a Shapiro-Test for Normality.
shapiro.test(y1)

# For alpha level of .05 significance we reject
# the Null Hypothesis, so the data are not assumed to be normal.

# Lets try and take the diffs in our data
dy1 <- diff(y1)


par(mfrow=c(1,2)) 
# Notes:
# 1) Mean of y1 = 0.0001 so almost no drift - offset.
# 2) Still some Heteroscedacity between the signal samples.

plot(dy1 ,type="l", col='blue', lwd=1, main="Time Series plot of HFRI Diff Monthly Returns", ylab="Monthly Returns", xlab="Years")
abline(h = mean(dy1), col='red')
legend("topleft",legend=c("HFRI values", "Mean of HFRI"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

# It seems almost normal with 2 outliers
hist(dy1, nclass=15, main="Histogram of Diff HFRI")

# Let's perform a Shapiro-Test for Normality.
shapiro.test(dy1)
# For alpha level of .05 significance we again reject
# the Null Hypothesis, but the data are now closer to normality
# Note that any other transformation failed.

par=(mfrow=c(1,1))
qqnorm(dy1,main="Normal QQplot of Diff HFRI")      # normal Q-Q plot  
qqline(dy1) # add a line   


par(mfrow=c(1,2)) # set up the graphics
acf(x=ts(dy1, freq=1), lag.max = 60, main="ACF of HFRI")   # Autocorrelation function plot
pacf(x=ts(dy1, freq=1), lag.max = 60, main="PACF of HFRI") # Partial Autocorrelation function


# We see that the significant terms are:
# MA - Terms 1:48
# AR - Terms 1-6:8

# Based on these plots EMA seems to be <<dumping>> shocks and 
# be almost stationary, lets see it with a test.

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(dy1,  lag=i, type="Ljung-Box")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," values"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(dy1^2,lag=i, type="Ljung-Box") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," values"))
  }
}


# Let's Perform a Dickey Fuller Test
# On a no-bias, we saw that the mean is near 0
# On a no-trend, since there is no visible trend
# On a lag-10 model.
m = ar(dy1)
m1=ur.df(dy1, type="none",lags=m$order-1)
summary(m1)

### We reject the null hypothesis 
### So the series is considered stationary


### Estimation Step

# MA-1 model
ma1fit=arima(dy1,order=c(0,0,1))
ma1fit
# Very significant coeff


# MA-2 model
ma2fit=arima(dy1,order=c(0,0,2), fixed=c(0,NA,NA))
ma2fit
# Coeff is not significant

# MA-1-2 model
ma1_2fit=arima(dy1,order=c(0,0,2))
ma1_2fit
# Better aic


# AR-1 model
ar1fit=arima(dy1,order=c(1,0,0))
ar1fit

# AR-2 model
ar2fit=arima(dy1,order=c(2,0,0), 
             fixed=c(0,NA,NA))
ar2fit


# AR1-2 model
ar1_2fit=arima(dy1,order=c(2,0,0))
ar1_2fit

#AR2MA2 model
ar2ma2fit=arima(dy1,order=c(2,0,2), fixed=c(NA,NA,0,NA,NA))
ar2ma2fit


#AR1MA1 model
ar1ma1fit = arima(dy1, order=c(1,0,1), fixed=c(NA,NA,0))
ar1ma1fit

#AR8MA1 model
ar8ma1fit = arima(dy1, order=c(8,0,1))
ar8ma1fit

### Best Model: AR8MA1
ar8ma1res=ar8ma1fit$residuals


par(mfrow=c(3,2)) # set up the graphics

acf(ts(ar8ma1res,freq=1), 60, main="ACF of residuals")

pacf(ts(ar8ma1res,freq=1), 60, main="PACF of residuals")

acf(ts(ar8ma1res^2,freq=1), 60, main="ACF of squared residuals")

pacf(ts(ar8ma1res^2,freq=1), 60, main="PACF of squared residuals")

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar8ma1res,  lag=i, type="Ljung-Box")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," values"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar8ma1res^2,lag=i, type="Ljung-Box") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," values"))
  }
}
# Let's perform a Shapiro-Test for Normality on the Residuals.
shapiro.test(ar8ma1res)
# Residuals are far from Normal due to outliers

qqnorm(ar8ma1res, main="Normal QQplot of residuals")
qqline(ar8ma1res) 


par(mfrow=c(1,1)) # set up the graphics
# Predictions
forecast=predict(ar8ma1fit, 10)

### Take the last item from the real series
prev = y1[189]

### And fix the predictions so the show real forecasts and not diffs
for (index in seq(1,10)){
  forecast$pred[index] = forecast$pred[index] + prev
  prev = forecast$pred[index]
}


UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se

minx = min(y1,LL); maxx = max(y1,UL)

ts.plot(y1, forecast$pred, gpars=list(main="Time Series plot of HFRI Monthly Returns", ylab="Monthly Returns", xlab="Years"))

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
fit <- lm(dy1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15, data=data[2:nrow(data),])

# Best AIC Model
model_AIC <- stepAIC(fit, direction="backward")

#Check Autocorrelation of the residuals and heteroscedasticity
AIC_res<-residuals(model_AIC)

par(mfrow=c(3,2))
shapiro.test(AIC_res)
# Residuals are not Normal
qqnorm(AIC_res,main="QQplot of Best AIC Model Residuals")  
qqline(AIC_res)

acf(AIC_res,lag.max=60, main="ACF of Best AIC Model Residuals")      
pacf(AIC_res,lag.max=60, main="PACF of Best AIC Model Residuals")    
acf(AIC_res^2,lag.max=60, main="ACF of Best AIC Model Residuals^2")  
pacf(AIC_res^2,lag.max=60, main="PACF of Best AIC Model Residuals^2") 

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(AIC_res,  lag=i, type="Ljung-Box")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(AIC_res^2,lag=i, type="Ljung-Box") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}



# Best BIC Model
model_BIC <- stepAIC(fit, direction = "backward", k =log(nrow(data)-1))

#Check Autocorrelation of the residuals and heteroscedasticity
BIC_res<-residuals(model_BIC)

par(mfrow=c(3,2))
shapiro.test(BIC_res)
# Residuals are not Normal
qqnorm(BIC_res,main="QQplot of Best BIC Model Residuals")  
qqline(BIC_res)

acf(BIC_res,lag.max=60, main="ACF of Best BIC Model Residuals")       
pacf(BIC_res,lag.max=60, main="PACF of Best BIC Model Residuals")     # Major Issues at lag 23
acf(BIC_res^2,lag.max=60, main="ACF of Best BIC Model Residuals^2")   
pacf(BIC_res^2,lag.max=60, main="PACF of Best BIC Model Residuals^2") 

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(BIC_res,  lag=i, type="Ljung-Box")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(BIC_res^2,lag=i, type="Ljung-Box") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}

##################################################
### Fixing Autocorrelation in Regression Model ###
##################################################

### In order to fix the Autocorrelation
### Candidate AR Terms 23

ar23_ma=arima(BIC_res,order=c(23,0,0), fixed=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA))
ar23_ma_res = ar23_ma$residuals

par(mfrow=c(3,2))
shapiro.test(ar23_ma_res)
# Residuals are not Normal
qqnorm(ar23_ma_res,main="QQplot of Best BIC Model Residuals")  
qqline(ar23_ma_res)

acf(ar23_ma_res,    lag.max=60, main="ACF of Best BIC Model Residuals")
pacf(ar23_ma_res,   lag.max=60, main="PACF of Best BIC Model Residuals")
acf(ar23_ma_res^2,  lag.max=60, main="ACF of Best BIC Model Residuals^2")  
pacf(ar23_ma_res^2, lag.max=60, main="PACF of Best BIC Model Residuals^2") 

for(i in c(1,2,3,5,10,20,60)){
  result = Box.test(ar23_ma_res,  lag=i, type="Ljung")
  if (result$p.value < 0.05){
    print(paste0("There exists autocorrelation between the first ",i," residuals"))
  }
}

for(i in c(1,2,3,5,10,20,25,30,60)){
  result = Box.test(ar23_ma_res^2,lag=i, type="Ljung") 
  if (result$p.value < 0.05){
    print(paste0("There exists heteroscedacity between the first ",i," residuals"))
  }
}

### There exists heteroscedasticity between the first 2-60 lags.
### Fix it with GARCH / ARCH modeling !
#######################################
# Estimate ARCH(2) model
arch_2=garchFit(~garch(2,0),data=ar23_ma_res,trace=F) # trace = F reduces the summary
summary(arch_2) 

# Estimate GARCH(2,2) model
garch_2_2=garchFit(~garch(2,2), data=ar23_ma_res, trace=F) # trace = F reduces the summary
summary(garch_2_2) 


# Estimate ARCH(2) model with Student Distribution
arch_2_t=garchFit(~garch(2,0),data=ar23_ma_res, trace=F,cond.dist="std") # trace = F reduces the summary
summary(arch_2_t)

arch_2_t_res = residuals(arch_2_t)
qqnorm(arch_2_t_res,main="QQplot of ARCH 2 Model Residuals")  
qqline(arch_2_t_res)


# Estimate GARCH(2,2) model with Student Distribution
garch_2_2_t=garchFit(~garch(2,2), data=ar23_ma_res, trace=F,cond.dist="std") # trace = F reduces the summary
summary(garch_2_2_t)

garch_2_2_t_res = residuals(garch_2_2_t)
qqnorm(garch_2_2_t_res,main="QQplot of GARCH 2,2 T Residuals")  
qqline(garch_2_2_t_res)


arch_2_10 =garchFit(~garch(2,10),data=ar23_ma_res,trace=F) # trace = F reduces the summary
summary(arch_2_10) 
arch_2_10_res = residuals(arch_2_10)
qqnorm(arch_2_10_res,main="QQplot of ARCH 2 10 Residuals")  
qqline(arch_2_10_res)

###########################################################
### Final Complete Model LM(x1,X2,x5,x7,x8,x15) + ARMA(23,0) + ARCH(2,10) ###
##########################################################
###############
coef(model_BIC)
###############


