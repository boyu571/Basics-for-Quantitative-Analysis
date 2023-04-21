# Heteroskedasticity, chapter 13

getwd()
setwd("D:/2Teaching/0BasicQuant/R_Quant_updated")
dir()

Housing <- read.csv("DataHousingPrice.csv")

install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
library(car)
library(sandwich)
library(lmtest)


ols1 = lm(PRICE~LOTSIZE+SQRFT+BDRMS,data=Housing)
summary(ols1)


#1. Breusch-Pagan test for heteroskedasticity
?bptest
bptest(ols1, ~LOTSIZE+SQRFT+BDRMS, data = Housing)
# 동분산이라는 귀무가설을 기각

uhat = resid(ols1)
# or uhat = ols1$residuals
aux = lm(I(uhat^2)~LOTSIZE+SQRFT+BDRMS, data=Housing)
n = nrow(Housing)
bpstat = n*summary(aux)$r.squared
bpstat
1-pchisq(bpstat,3) # k는 설명변수의 개수


# Note that you need I() for a squared explanatory variable 
lm(PRICE~I(LOTSIZE^2), data = Housing)
lm(PRICE~LOTSIZE^2, data = Housing)
lm(PRICE~LOTSIZE, data = Housing)


# Taking logs often helps to secure homoscedasticity

ols2 = lm(log(PRICE)~log(LOTSIZE)+log(SQRFT)+BDRMS,data=Housing)

# Conduct the Breusch-Pagan test for heteroskedasticity
bptest(ols2, ~log(LOTSIZE)+log(SQRFT)+BDRMS, data = Housing)
# 동분산이라는 귀무가설을 기각하지 못함. 동분산

uhat = resid(ols2)
# or uhat = ols1$residuals
aux = lm(I(uhat^2)~log(LOTSIZE)+log(SQRFT)+BDRMS, data=Housing)
n = nrow(Housing)
bpstat = n*summary(aux)$r.squared
bpstat
1-pchisq(bpstat,3) # k는 설명변수의 개수



#2. Heteroskedasticity robust standard error

?vcovHC





#3. Weighted Least Squares EStimation

set.seed(101)
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
u = x1*rnorm(n)
plot(u~x1)

y = 1 + x1 - x2 + u
ols3=lm(y~x1+x2)
summary(ols3)

#R parameterizes the weights as inversely proportional to the variances.
#u = x1*rnorm(n)  var(u)=x1^2






#Feasible GLS for unknown form of heteroskedasticity

ols1 = lm(PRICE~LOTSIZE+SQRFT+BDRMS,data=Housing)

uhat = resid(ols1)
olsuhat = lm(I(log(uhat^2))~LOTSIZE+SQRFT+BDRMS,data=Housing)

h = exp(fitted(olsuhat))  #estimate of variance of the error term


# WLS using weights=1/h

fgls = 
  coeftest(fgls)



#####
# 예제 13.5
install.packages("loedata")
library(loedata)
data(Death)
ols4 = lm(deathrate~drink+smoke+aged+vehipc+factor(year),data=Death)
summary(ols4)



# Test for heteroskedasticity
bptest(ols4, ~drink+smoke+aged+vehipc+factor(year),data=Death)
# 동분산

# heteroskedasticity robust standard error 


# WLS using regpop (지역 인구) 예제 13.8
wls2 = lm(deathrate~drink+smoke+aged+vehipc+factor(year),weights=regpop,data=Death)
summary(wls2)


# FGLS for unknonwn

