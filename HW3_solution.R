#Homework 3


smoke <- read.csv("smoke.csv")

n    = nrow(smoke)

cigs= smoke[,1]
income= smoke[,2]
cigpric= smoke[,3]
educ= smoke[,4]
age= smoke[,5]
agesq= smoke[,6]
restaurn= smoke[,7]

ols1=lm(cigs~log(income)+log(cigpric)+educ+age+agesq+restaurn,data=smoke)
summary(ols1)


#install.packages("car")
#library(car)

install.packages("lmtest")
library(lmtest)


install.packages("sandwich")
library(sandwich)

bptest(ols1,~log(income)+log(cigpric)+educ+age+agesq+restaurn,data=smoke)

coeftest(ols1,vcov=vcovHC)

#FGLS
uhat = resid(ols1)
ols2 = lm(I(log(uhat^2))~log(income)+log(cigpric)+educ+age+agesq+restaurn,data=smoke)

h = exp(fitted(ols2))

fgls = lm(cigs~log(income)+log(cigpric)+educ+age+agesq+restaurn,weight=1/h,data=smoke)
coeftest(fgls)


#####################
fish <- read.csv("fish.csv")

n    = nrow(smoke)

avgprc = fish[,1]
mon = fish[,2]
tues = fish[,3]
wed = fish[,4]
thurs = fish[,5]
t = fish[,6]
wave2 = fish[,7]
wave3 = fish[,8]

ols1 = lm(log(avgprc)~mon+tues+wed+thurs+t,data=fish)
summary(ols1)

ols2 = lm(log(avgprc)~mon+tues+wed+thurs+t+wave2+wave3,data=fish)
summary(ols2)

bgtest(ols2)

coeftest(ols2,vcov=vcovHAC)



#iterated Cochrane-Orcutt estimation

install.packages("orcutt")
library("orcutt")

co=cochrane.orcutt(ols2, convergence = 8, max.iter=100)
summary(co)


#################
fertil2 <- read.csv("fertil2.csv")

n    = nrow(fertil2)

children=fertil2[,1]
age=fertil2[,2]
educ=fertil2[,3]
frsthalf=fertil2[,4]
tv=fertil2[,5]

ols1 = lm(children~educ+age+I(age^2),data=fertil2)
coeftest(ols1)

ols2 = lm(educ~age+I(age^2)+frsthalf,data=fertil2)
coeftest(ols2)

install.packages("AER")
library(AER)

#2SLS

tsls = ivreg(children~educ+age+I(age^2)|age+I(age^2)+frsthalf,data=fertil2)
coeftest(tsls)


ols3 = lm(children~educ+age+I(age^2)+tv,data=fertil2)
coeftest(ols3)
tsls2 = ivreg(children~educ+age+I(age^2)+tv|age+I(age^2)+frsthalf+tv,data=fertil2)
coeftest(tsls2)



