library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(orcutt)
library(AER)

# 1
gpa1 = read.csv("gpa1.csv")
gpa1
nrow(gpa1)

complete.cases(gpa1)
#na.omit returns the object with incomplete cases removed.
data = na.omit(gpa1)
n    = nrow(gpa1)
colnames(gpa1)

# a)
ols1 = lm(colGPA ~ hsGPA + ACT + skipped + PC, data = gpa1)
summary(ols1)
# colGPA = 1.357 +0.413hsGPA +0.013ACT -0.071skipped +0.124PC
#         (0.328)  (0.09)      (0.010)     (0.026)    (0.057)  
# at 5% level, variable hsGPA, skipped, PC are statistically significant.

# b)
bptest(ols1,~ hsGPA + ACT + skipped + PC, data = gpa1)
# p-value = 0.01831으로 5%에서 동분산이라는 귀무가설을 기각

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
ols1uhat = lm(I(log(uhat^2))~ hsGPA + ACT + skipped + PC, data = gpa1)
h = exp(fitted(ols1uhat))  #estimate of variance of the error term

# WLS using weights=1/h

fgls = lm(colGPA ~ hsGPA + ACT + skipped + PC,weights=1/h, data = gpa1)
coeftest(fgls)
coeftest(ols1)
summary(fgls)
# colGPA = 1.454 +0.370hsGPA +0.016ACT -0.086skipped +0.125PC
#         (0.287)  (0.077)    (0.009)     (0.021)    (0.06) 



# 2
fertil3final = read.csv("fertil3final.csv")
fertil3final
nrow(gpa1)

complete.cases(fertil3final)
#na.omit returns the object with incomplete cases removed.
data = na.omit(fertil3final)
n    = nrow(fertil3final)
colnames(fertil3final)

# a)
ols2 = lm(cgfr ~ cpe + cpe1 + cpe2, data = fertil3final)
summary(ols2)
# cgfr = -0.946 -0.0356cpe -0.014cpe1 +0.110cpe2 
#        (0.489)  (0.028)    (0.028)   (0.028)   
# variable cpe2 is statistically significant.

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
wagefinal = read.csv("wagefinal.csv")
wagefinal
nrow(wagefinal)

complete.cases(wagefinal)
#na.omit returns the object with incomplete cases removed.
data = na.omit(wagefinal)
n    = nrow(wagefinal)
colnames(wagefinal)

# a)
ols3 = lm(log(wage) ~ educ + exper + tenure + black, data = wagefinal)
summary(ols3)
# colGPA = 5.569 +0.0713educ +0.0180exper +0.0095tenure -0.17615black
#        (0.1248)  (0.0072)    (0.0039)     (0.0030)       (0.0505) 

# b)
# We think that educ is endogenous. 
# sibs are assumed to be exogenous.
# Is 'Frsthalf' reasonable IV candidate for educ?
ols_sibs = lm(educ ~ exper + tenure + black + sibs, data = wagefinal)
coeftest(ols_sibs)
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

tsls = ivreg(log(wage) ~ educ + exper + tenure + black|exper + tenure + black + sibs, data = wagefinal)
summary(tsls)
summary(ols3)

# d)
tsls2 = ivreg(log(wage) ~ educ + exper + tenure + black|exper + tenure + black + sibs + meduc + feduc, data = wagefinal)
summary(tsls2)



# 5-2
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

ar1=arima(bondyields$, order=c(1,0,0))
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



