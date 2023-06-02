
### Unit Root Test ###

# ADF, PP and KPSS Tests using "urca" package

# getwd()
# setwd("C:/Users/Heejoon/Desktop/R")
# dir()

install.packages("urca")
install.packages("readxl")
library(urca)
library(readxl)


data <- read_excel("DefaultPremium_data.xls")
head(data)

daily = matrix(data$Daily)
weekly = matrix(data$Weekly)
monthly = matrix(data$Monthly)

daily = na.omit(daily)
weekly = na.omit(weekly)
monthly = na.omit(monthly)

plot(daily)
plot(weekly)
plot(monthly)

### 0. Check Estimates of AR Coefficients 

ar1=arima(daily, order=c(1,0,0))
ar1

ar2=arima(daily, order=c(1,0,0), xreg = 1:length(daily))
ar2
# xreg가 linear trend 반영


### 1. Augmented Dickey-Fuller (ADF) Test
## Null = Unit Root

?ur.df

# Model with intercept
daily_adf1=ur.df(daily, type="drift")
# type = drift는 intercept만 들어간것
summary(daily_adf1)

daily_adf1@teststat
# tau2 값을 보면 됨.
daily_adf1@cval
daily_adf1@lags

daily_adf1@pvalue


# Another package for unit root testing (for p-value)
#install.packages("tseries")
#library(tseries)

#adf.test(daily)
#?adf.test
#pp.test(daily)
#kpss.test(daily)


# Model with intercept and trend
daily_adf2=ur.df(daily, type="trend")
# type = trend는 trend 포함
daily_adf2
summary(daily_adf2)


### 2. Phillips-Perron (PP) Test
## Null = Unit Root

?ur.pp

# Model with intercept
daily_pp1=ur.pp(daily, type = "Z-tau" , model = "constant")
daily_pp1
summary(daily_pp1)

# Model with intercept and trend
daily_pp2=ur.pp(daily, type = "Z-tau" , model = "trend")
daily_pp2
summary(daily_pp2)


## Note that the critical values for ADF and PP tests are the same. 
## Both tests have the same asymptotic distribution under the null hypothesis

daily_adf1@cval
daily_adf2@cval
daily_pp1@cval
daily_pp2@cval


### 3. Kwiatkowsk-Phillips-Schmidt-Shin (KPSS) Test 
## Null= Stationarity

?ur.kpss

# Model with intercept
daily_kpss1=ur.kpss(daily, type="mu", lags = "long")
daily_kpss1
# 2.25
summary(daily_kpss1)
# critical values 0.347 0.463  0.574 0.739
# reject

# Model with intercept and trend
daily_kpss2=ur.kpss(daily, type="tau", lags = "long" )
daily_kpss2
summary(daily_kpss2)


## Excerciae: Repeat with Weekly & Monthly Data
## In particular, get the results for the monthly data. Is it stationary or integrated? 


### 0. Check Estimates of AR Coefficients 

ar1=arima(monthly, order=c(1,0,0))
ar1

ar2=arima(monthly, order=c(1,0,0), xreg = 1:length(monthly))
ar2
# xreg가 linear trend 반영


### 1. Augmented Dickey-Fuller (ADF) Test
## Null = Unit Root

?ur.df

# Model with intercept
monthly_adf1=ur.df(monthly, type="drift")
# type = drift는 intercept만 들어간것
summary(monthly_adf1)

monthly_adf1@teststat
# tau2 값을 보면 됨.
monthly_adf1@cval
monthly_adf1@lags

monthly_adf1@pvalue


# Another package for unit root testing (for p-value)
#install.packages("tseries")
#library(tseries)

#adf.test(daily)
#?adf.test
#pp.test(daily)
#kpss.test(daily)


# Model with intercept and trend
monthly_adf2=ur.df(monthly, type="trend")
# type = trend는 trend 포함
monthly_adf2
summary(monthly_adf2)


### 2. Phillips-Perron (PP) Test
## Null = Unit Root

?ur.pp

# Model with intercept
monthly_pp1=ur.pp(monthly, type = "Z-tau" , model = "constant")
monthly_pp1
summary(monthly_pp1)

# Model with intercept and trend
monthly_pp2=ur.pp(monthly, type = "Z-tau" , model = "trend")
monthly_pp2
summary(monthly_pp2)


## Note that the critical values for ADF and PP tests are the same. 
## Both tests have the same asymptotic distribution under the null hypothesis

monthly_adf1@cval
monthly_adf2@cval
monthly_pp1@cval
monthly_pp2@cval


### 3. Kwiatkowsk-Phillips-Schmidt-Shin (KPSS) Test 
## Null= Stationarity

?ur.kpss

# Model with intercept
monthly_kpss1=ur.kpss(monthly, type="mu", lags = "long")
monthly_kpss1
# 1.12
summary(monthly_kpss1)
# critical values 0.347 0.463  0.574 0.739
# reject

# Model with intercept and trend
monthly_kpss2=ur.kpss(monthly, type="tau", lags = "long" )
monthly_kpss2
summary(monthly_kpss2)







##Exercise 2: Try Unit root tests
data = as.matrix(read.table("q-gdp4708.txt",header=T))
head(data)

gdp = matrix(data[,4])
y = log(gdp)

