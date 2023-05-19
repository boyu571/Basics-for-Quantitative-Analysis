# IV Estimation

dir()

#install.packages("car")
#install.packages("sandwich")
#install.packages("lmtest")

library(car)
library(sandwich)
library(lmtest)


WAGEIV = read.csv("mrozdata.csv")
attach(WAGEIV)
detach(WAGEIV)

# Run the regression of log(wage) on educ, exper, exper^2
ols1 = lm(log(wage) ~ educ + exper+ I(exper^2), data = WAGEIV)
summary(ols1)
coeftest(ols1)

install.packages("AER")
library(AER)


# We think that educ is endogenous. 
# exper, motheduc, and fatheduc are assumed to be exogenous.

# Is 'motheduc' reasonable IV candidate for educ?
ols_m=lm(educ ~ exper + I(exper^2)+motheduc, data = WAGEIV)
coeftest(ols_m)
# motheduc가 correlate되어 있다.

# Is 'fatheduc' reasonable IV candidate for educ?
ols_f=lm(educ ~ exper + I(exper^2)+fatheduc, data = WAGEIV)
coeftest(ols_f)

  
    

#2SLS estimation using motheduc and fatheduc as IVs

tsls = ivreg(log(wage)~educ+exper+I(exper^2)|exper+I(exper^2)+motheduc+fatheduc, data = WAGEIV)
summary(tsls)
coeftest(tsls)



#2 stage regressions

#1st stage
firststage = lm(educ~exper+I(exper^2)+motheduc+fatheduc, data = WAGEIV)
fittededuc = fitted(firststage)
#2nd stage
secondstage=lm(log(wage)~fittededuc+exper+I(exper^2), data = WAGEIV)
coeftest(secondstage)
# Note that 1) the estimates are same as 'tsls' but 2) standard errors are different. standard errors should be those in 'tsls'.





#Test whether educ is exogenous or endogenous
ols2 = lm(educ~exper+I(exper^2)+motheduc+fatheduc,data=WAGEIV)
vhat = resid(ols2)

ols3 = lm(log(wage)~educ+exper+I(exper^2)+vhat,data=WAGEIV)
summary(ols3)
# 10% 유의수준에서 endogenous, 5% 유의수준에서 exogenous

########

phillips <- read.csv("phillips.csv")

n    = nrow(phillips)

unem=phillips[,1]
inf=phillips[,2]

dinf = diff(inf)
unem = as.matrix(unem)
head(unem)

original_pc = lm(inf~unem, data = phillips)
coeftest(original_pc)
#dos not make sense

# Run the regression of dinf_t on unem_t
ols4 = lm(dinf~unem[2:n], data = phillips)
summary(ols4)
coeftest(ols4)


# Is unem_(t-1) a reasonable IV candidate for unem_t
ols5 = lm(unem[2:n]~unem[1:n-1], data=phillips)
coeftest(ols5)
#unem(t)~unem(t-1)


#library(AER)

#2SLS using unem_(t-1) as an IV

tsls2 = ivreg(dinf~unem[2:n]|unem[1:(n-1)], data = phillips)
coeftest(tsls2)
#ols5랑 비교했을때 차이가 있음

# expectation augmented phillips curve
