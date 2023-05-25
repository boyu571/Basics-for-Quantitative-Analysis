library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(orcutt)
library(AER)

# 1
smoke = read.csv("smoke.csv")
smoke
nrow(smoke)

complete.cases(smoke)
#na.omit returns the object with incomplete cases removed.
data = na.omit(smoke)
n    = nrow(smoke)
colnames(smoke)
ols1 = lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn, data = smoke)
summary(ols1)

# a)
# cigs = -3.64 + 0.88log(income) -0.75log(cigpric) -0.50educ + 0.77age - 0.01agesq - 2.82restaurn
#       (24.08)     (0.72)            (5.77)         (0.17)    (0.16)     (0.002)        (1.11)
# at 5% level, variable educ, age, agesq, restaurn are statistically significant.

# b)
bptest(ols1,~ log(income) + log(cigpric) + educ + age + agesq + restaurn, data = smoke)
# p-value = 1.456e-05으로 동분산이라는 귀무가설을 기각

# c)
coeftest(ols1, vcov=vcovHC)
# cigs = -3.64 + 0.88log(income) -0.75log(cigpric) -0.50educ + 0.77age - 0.01agesq - 2.82restaurn
#       (25.86)     (0.60)            (6.09)         (0.16)    (0.14)     (0.001)        (1.01)
# Except log(cigpric), heteoskedasticity-robust standard errors are smaller than usual standard error.

# d)
#Feasible GLS for unknown form of heteroskedasticity
uhat = resid(ols1)
uhat
ols1uhat = lm(I(log(uhat^2))~ log(income) + log(cigpric) + educ + age + agesq + restaurn, data = smoke)
h = exp(fitted(ols1uhat))  #estimate of variance of the error term

# WLS using weights=1/h

fgls = lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn, weights=1/h, data = smoke)
coeftest(fgls)
coeftest(ols1)
summary(fgls)
# cigs = 5.64 + 1.30log(income) -2.95log(cigpric) -0.46educ + 0.48age - 0.0056agesq - 3.46restaurn
# p-value of log(income) is 0.003128, so it is statiscally significant even in 1% level.



# 2
fish = read.csv("fish.csv")
fish

complete.cases(fish)
#na.omit returns the object with incomplete cases removed.
data = na.omit(fish)
n    = nrow(fish)
colnames(fish)

# 1)
ols2 = lm(log(avgprc)~ mon + tues+ wed+ thurs+t, data = fish)
summary(ols2)
# log(avgprc) = -0.073 -0.0101mon -0.0088tues + 0.0376wed + 0.0906thurs - 0.004t

ols21 = lm(log(avgprc)~ t, data = fish)
anova(ols2, ols21)
# The test for joint significance of the day-of-the-week dummies is F = 0.23, which gives p-value = 0.92.
# So there is no evidence that the average price of fish varies systematically within a week.

# 2)
ols3 = lm(log(avgprc)~ mon + tues+ wed+ thurs+t + wave2 + wave3, data = fish)
summary(ols3)
# log(avgprc) = -0.92 -0.0182mon -0.0085tues + 0.05wed + 0.1225thurs -0.0012t+0.0909wave2 +0.0474wave3
# wave2 and wave3 are individually significant in 1% level. Rough seas(as measure by high waves) would reduce the supply of fish(shift the supply curve back), and this would result in a price increse. One might argue that bad weather reduces the demand for fish at a market, too, but that would reduce price. If there are demand effects captured by the wave variables, they are being swamped by the supply effects.

# 3)
dwtest(ols3)
# DW = 0.74523, p-value = 2.654e-12, auto-correlation이 없다는 것을 reject함
bgtest(ols3, order=1)
# p-value = 9.882e-10, auto-correlation이 없다는 것을 reject함

# 4)
# The Newey-West HAC, robust standard error
coeftest(ols3, vcov = vcovHAC)
summary(ols3)
# The The Newey-West standard errors are not much different with the usual, incorrect standard errors.

# 5)
co=cochrane.orcutt(ols3, convergence = 8, max.iter=100)
summary(co)
# wave2 and wave3 are individually significant in 5% level.



# 3
fertil2 = read.csv("fertil2.csv")
fertil2

complete.cases(fertil2)
#na.omit returns the object with incomplete cases removed.
data = na.omit(fertil2)
n    = nrow(fertil2)
colnames(fertil2)

# 1)
ols4 = lm(children ~ educ + age + I(age^2), data = fertil2)
summary(ols4)
# Holding age fixed, as one year increases on education there's 0.09 decrease in fertility(number of children).
# If 100 women receive another year of education, 9 less children are expected to have.

# 2)
# We think that educ is endogenous. 
# age, Frsthalf are assumed to be exogenous.
# Is 'Frsthalf' reasonable IV candidate for educ?
ols_frs = lm(educ ~ age + I(age^2) + frsthalf, data = fertil2)
coeftest(ols_frs)
# frsthalf is correlated to educ so frsthalf is reasonable IV for educ

# 3)
#1st stage
ols_frs = lm(educ ~ age + I(age^2) + frsthalf, data = fertil2)
fittededuc = fitted(ols_frs)
#2nd stage
ols5 = lm(children ~ fittededuc + age + I(age^2), data = fertil2)
coeftest(ols5)
# Note that 1) the estimates are same as 'tsls' but 2) standard errors are different. standard errors should be those in 'tsls'.
summary(ols4)
# OLS estimate of education is -0.09 and IV estimate of education is -0.17.
# Using frsthalf as an IV for educ, estimated effect has increased.

# same result
tsls = ivreg(children ~ educ + age + I(age^2)|age + I(age^2) + frsthalf, data = fertil2)
summary(tsls)
coeftest(tsls)

# 4)
ols_tv= lm(educ ~ age + I(age^2) + tv, data = fertil2)
coeftest(ols_tv)
# tv is correlated to educ. so 'tv' is reasonable IV for educ

tsls2 = ivreg(children ~ educ + age + I(age^2)|age + I(age^2) + tv, data = fertil2)
summary(tsls2)
coeftest(tsls2)
summary(ols4)
# OLS estimate of education is -0.09 and IV estimate of education is -0.17.
# The result support the claim that tv ownership has a negative effect on fertility by effecting on educ.