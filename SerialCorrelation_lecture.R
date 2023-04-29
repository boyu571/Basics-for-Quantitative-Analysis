#Serial Correlation

minwage <- read.csv("prminwge.csv")

n    = nrow(minwage)

prepop = minwage[,1]
mincov = minwage[,2]
usgnp = minwage[,3]

# Run the regression of log(prepop) on log(mincov) and log(usgnp)
ols1 = lm(log(prepop)~log(mincov)+log(usgnp))
summary(ols1)


#Durbin-Watson test
library("lmtest")
dwtest(ols1)

#checking Durbin-Watson test
uhat = resid(ols1)
sum(diff(uhat)^2/sum(uhat^2))


#Breusch-Godfrey test
bgtest(ols1)
# auto-correlation이 없다는 것을 reject함

# for AR(3)
bgtest(ols1, order=3)


install.packages("sandwich")
library(sandwich)

#Newey-West HAC, robust standard error
coeftest(ols1, vcov = vcovHAC)
coeftest(ols1) 


#Cochrane-Orcutt estimation

uhat[2:n]
head(uhat)
head(uhat[2:n])
tail(uhat)
tail(uhat[2:n])

ols2 = lm(uhat[2:n]~uhat[1:(n-1)])
summary(ols2)
rho = coefficients(ols2,2)
rho = rho[2]

ystar = log(prepop[2:n])-rho*log(prepop[1:(n-1)])
x1star = log(mincov[2:n])-rho*log(mincov[1:(n-1)])
x2star = log(usgnp[2:n])-rho*log(usgnp[1:(n-1)])

cochraneorcutt = lm(ystar~x1star+x2star)  
summary(cochraneorcutt)
coeftest(cochraneorcutt)


# iterated Cochrane-Orcutt: iterating until convergence

install.packages("orcutt")
library("orcutt")

co=cochrane.orcutt(ols1, convergence = 8, max.iter=100)
summary(co)

############
traffic <- read.csv("traffic2.csv")

n    = nrow(traffic)

prcfat=traffic[,1]
t=traffic[,2]
wkends=traffic[,3]
spdlaw=traffic[,4]
beltlaw=traffic[,5]
unem=traffic[,6]

feb=traffic[,7]
mar=traffic[,8]
apr=traffic[,9]
may=traffic[,10]
jun=traffic[,11]
jul=traffic[,12]
aug=traffic[,13]
sep=traffic[,14]
oct=traffic[,15]
nov=traffic[,16]
dec=traffic[,17]


ols2 = lm(prcfat~spdlaw+beltlaw+unem+wkends+t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,data=traffic)
coeftest(ols2)


# Conduct Breusch-Godfrey test for AR(1)
bgtest(ols2)

# Use the robust s.e. using the Newey-West HAC 
coeftest(ols2, vcov = vcovHAC)

# Iterated Cochrane-Orcutt estimation
co2=cochrane.orcutt(ols2, convergence = 8, max.iter=100)
summary(co2)

?cochrane.orcutt
