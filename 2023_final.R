library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(orcutt)
library(AER)
library(urca)
library(dplyr)

# 1
crime1 = read.csv("CRIME1.csv")
crime1
nrow(crime1)

complete.cases(crime1)
#na.omit returns the object with incomplete cases removed.
data = na.omit(crime1)
n    = nrow(crime1)
colnames(crime1)

ols1 = lm(narr86 ~ pcnv + avgsen + tottime + ptime86 +qemp86, data = crime1)
summary(ols1)

# b)
bptest(ols1,~ pcnv + avgsen + tottime + ptime86 +qemp86, data = crime1)
# p-value = 3.546e-07으로 5%에서 동분산이라는 귀무가설을 기각

# c)
coeftest(ols1, vcov=vcovHC)
# colGPA = 1.357 +0.413hsGPA +0.013ACT -0.071skipped +0.124PC
#         (0.355)  (0.102)      (0.011)     (0.028)    (0.06) 
# variable skipped became insignificant at the 1% level.
# variable hsGPA is statically significant.

# d)
#Feasible GLS for unknown form of heteroskedasticity
uhat = resid(ols1)
uhat
ols1uhat = lm(I(log(uhat^2))~ pcnv + avgsen + tottime + ptime86 +qemp86, data = crime1)
h = exp(fitted(ols1uhat))  #estimate of variance of the error term

# WLS using weights=1/h

fgls = lm(narr86 ~ pcnv + avgsen + tottime + ptime86 +qemp86,weights = 1/h, data = crime1)
coeftest(fgls)
coeftest(ols1)
summary(fgls)
# colGPA = 1.454 +0.370hsGPA +0.016ACT -0.086skipped +0.125PC
#         (0.287)  (0.077)    (0.009)     (0.021)    (0.06) 

# e)
crime1 <-crime1 %>% mutate(
  narr86 = ifelse(narr86 == 0, 0, 1)
)
ols1 = lm(narr86 ~ pcnv + avgsen + tottime + ptime86 +qemp86, data = crime1)
summary(ols1)

# MLE




# 2
barium = read.csv("BARIUM.csv")
barium
nrow(barium)

complete.cases(barium)
#na.omit returns the object with incomplete cases removed.
data = na.omit(barium)
n    = nrow(barium)
colnames(barium)

ols2 = lm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 +affile6 +afdec6, data = barium)
summary(ols2)

# b)
bgtest(ols2, order=1)
# p-value = 0.01666,5% 유의수준에서 auto-correlation이 없다는 것을 reject함
# serial correlation이 있다.

# c)
# The Newey-West HAC, robust standard error
coeftest(ols2, vcov = vcovHAC)
summary(ols2)
# no difference

# d)
co=cochrane.orcutt(ols2, convergence = 8, max.iter=100)
summary(co)
# variable cpe2 is statistically significant.


# 3
ksubs = read.csv("401ksubs.csv")
ksubs
nrow(ksubs)

complete.cases(ksubs)
#na.omit returns the object with incomplete cases removed.
data = na.omit(ksubs)
n    = nrow(ksubs)
colnames(ksubs)

ols3 = lm(pira ~ p401k + inc + I(inc^2) + age + I(age^2), data = ksubs)
summary(ols3)

# b)
# We think that p401k is endogenous. 
# e401k are assumed to be exogenous.
# Is 'e401k' reasonable IV candidate for educ?
ols_e401k = lm(p401k ~ inc + I(inc^2) + age + I(age^2) + e401k, data = ksubs)
coeftest(ols_e401k)
# sibs is correlated to educ so sibs is reasonable IV

# c)
# #1st stage
# ols_frs = lm(educ ~ age + I(age^2) + frsthalf, data = fertil2)
# fittededuc = fitted(ols_frs)
# #2nd stage
# ols5 = lm(children ~ fittededuc + age + I(age^2), data = fertil2)
# coeftest(ols5)
# # Note that 1) the estimates are same as 'tsls' but 2) standard errors are different. standard errors should be those in 'tsls'.
# summary(ols4)
# # OLS estimate of education is -0.09 and IV estimate of education is -0.17.
# # Using frsthalf as an IV for educ, estimated effect has increased.

tsls = ivreg(pira ~ p401k + inc + I(inc^2) + age + I(age^2)|inc + I(inc^2) + age + I(age^2) + e401k, data = ksubs)
summary(tsls)
summary(ols3)

# d)
#Test whether educ is exogenous or endogenous
# ols2 = lm(educ~exper+I(exper^2)+motheduc+fatheduc,data=WAGEIV)
# vhat = resid(ols2)

ols4 = lm(p401k ~ inc + I(inc^2) + age + I(age^2) + e401k, data = ksubs)
vhat = resid(ols4)

# ols3 = lm(log(wage)~educ+exper+I(exper^2)+vhat,data=WAGEIV)
# summary(ols3)

ols5 = lm(pira ~ p401k + inc + I(inc^2) + age + I(age^2) +vhat, data = ksubs)
summary(ols5)
# 10% 유의수준에서 endogenous, 5% 유의수준에서 exogenous






# 5-1
bondyields = read.csv("bondyields.csv")
bondyields
nrow(bondyields)

complete.cases(bondyields)
#na.omit returns the object with incomplete cases removed.
data = na.omit(bondyields)
n    = nrow(bondyields)
colnames(bondyields)

# a)
# 0. Check Estimates of AR Coefficients 

ar1=arima(bondyields$tbill, order=c(1,0,0))
ar1

ar2=arima(bondyields$tbill, order=c(1,0,0), xreg = 1:length(bondyields$tbill))
ar2
# xreg가 linear trend 반영


### 1. Augmented Dickey-Fuller (ADF) Test
## Null = Unit Root

?ur.df

# Model with intercept
monthly_adf1=ur.df(bondyields$tbill, type="drift")
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
monthly_adf2=ur.df(bondyields$tbill, type="trend")
# type = trend는 trend 포함
monthly_adf2
summary(monthly_adf2)
monthly_adf2@teststat

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
monthly_kpss1=ur.kpss(bondyields$tbill, type="mu", lags = "long")
monthly_kpss1
# 1.12
summary(monthly_kpss1)
# critical values 0.347 0.463  0.574 0.739
# reject

# Model with intercept and trend
monthly_kpss2=ur.kpss(bondyields$tbill, type="tau", lags = "long" )
monthly_kpss2
summary(monthly_kpss2)
