### Maximum Likelihood Estimation (MLE) ###
# Using 'mle2' function

install.packages("bbmle")
library(bbmle)
library(lmtest)


n=500
?rnorm
set.seed(14)
x = rnorm(n, 10, 2)
u = rnorm(n,0,1)
y = 1 + 2*x + u

plot(y~x)


## OLS regression

model.ols = lm(y~x)

coeftest(model.ols)

# Checking the distribution of residuals
uhat1 = resid(model.ols)
hist(uhat1, freq = TRUE, breaks=seq(-5, 5, by=0.05))

logLik(model.ols)

## MLE

# Defining the Log-likelihood Function

logftn = function(b0, b1, sigma2){
  
  Y.mean = b0 + b1*x
  
  log1 <- -0.5*n*log(2*pi) - 0.5*n*log(sigma2) - sum((y-Y.mean)^2)/(2*sigma2)
  
  return(-log1)
}

# Note that we put minus: maximizing log likelihood is identical to minimizing -log likelihood. 

model.mle <- mle2(logftn, start = list(b0=1, b1=0, sigma2=1))

?mle2

summary(model.mle)
logLik(model.mle)
confint(model.mle, level=0.95)



## Application to Housing Price Regression ##
install.packages("Ecdat")
library("Ecdat")

data(Housing,package="Ecdat")
summary(Housing)


# OLS regression 

ols3 = lm(log(price)~log(lotsize)+bedrooms,data=Housing)

coeftest(ols3)


# MLE

n = nrow(Housing)

logftn2 = function(b0, b1, b2, sigma2){
  
  Y.mean = b0 + b1*log(lotsize) + b2*bedrooms
  
  log2 <- -0.5*n*log(2*pi) - 0.5*n*log(sigma2) - sum((log(price)-Y.mean)^2)/(2*sigma2)
  
  return(-log2)
}


#,data=Housing
model.mle2 <- mle2(logftn2, start = list(b0=1, b1=0, b2=0, sigma2=1),data=Housing)

coeftest(model.mle2)
coeftest(ols3)
