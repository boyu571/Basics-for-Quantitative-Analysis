# Distributions

# distribution, cdf, quantile function, etc.


# Normal Distribution
?dnorm
# dnorm gives the density, pnorm gives the distribution function, qnorm gives the quantile function, and rnorm generates random deviates.

curve(dnorm(x,mean=0,sd=1),-6,6)

# 0.95 quantile of N(0,1), i.e., standard normal distribution
qnorm(0.95,mean=0,sd=1)  

# 0.05 quantile of N(0,1)?
qnorm(0.05,mean=0,sd=1)  

# What are critical values at the 10% significance level? 
qnorm(0.05,mean=0,sd=1)  


# Critical values at the 5% significance level for N(0,1)?
qnorm(0.025,mean=0,sd=1)  
# 0.975 quantile and 0.025 quantile of N(0,1), 
qnorm(0.975,mean=0,sd=1)  


# CDF for -1.96
pnorm(-1.96,mean=0,sd=1)

# CDF for 1.96
pnorm(1.96,mean=0,sd=1)

# p-value of 1.96 for two-sided test
2*(1-pnorm(1.96,mean=0,sd=1))
2*(1-pnorm(1.67,mean=0,sd=1))

# p-value of 1.96 for one-sided test. For example, H0: beta=0, H1: beta>0
1-pnorm(1.96,mean=0,sd=1)

qnorm(0.005,mean=0,sd=1)  
qnorm(0.01,mean=0,sd=1) 

# Chi-Squared Distribution
?dchisq
#dchisq gives the density, pchisq gives the distribution function, qchisq gives the quantile function, and rchisq generates random deviates.

curve(dchisq(x,df=5),0,12)
# Try various df (degrees of freedom)


# 0.95 quantile
qchisq(0.95,5)

a1 = qchisq(0.95,5)

# CDF of a1
pchisq(a1,5)

1-pchisq(a1,5)

1-pchisq(8.9,5)

# Suppose that a test statistic is chi-squared distributed and its degrees of freedom is 3. If the test statistic is very large, one rejects the null hypothesis in favor of the alternative hypothesis. If the test statistic is 9, what is the p-value?  
curve(dchisq(x,df=3),0,12)
1-pchisq(9, 3)
# 5% 유의수준에서는 reject 가능

# Student t Distribution
?dt
# dt gives the density, pt gives the distribution function, qt gives the quantile function, and rt generates random deviates.

curve(dt(x,df=1),-6,6)
curve(dt(x,df=5),-6,6)
curve(dt(x,df=10),-6,6)
curve(dt(x,df=50),-6,6)

# 0.95 quantile, df = 20
qt(0.95,20)


a2 = qt(0.95,20)

2*(1-pt(a2,df=20))


# F Distribution
?df
#df gives the density, pf gives the distribution function qf gives the quantile function, and rf generates random deviates.

curve(df(x,df1=2,df2=20),0,12)

qf(0.95, 2, 20)

a3 = qf(0.95, 2, 20)
pf(a3, 2, 20)



#####################
# Chapter 6


install.packages("Ecdat")
library("Ecdat")

data(Housing, package="Ecdat")

ols = lm(log(price)~log(lotsize),data=Housing)
summary(ols)


# critical value
qt(0.995,(546-2))
# p-value for two-sided test
2*(1-pt(16.61,546-2))

# confidence interval
confint(ols)
confint(ols, 'log(lotsize)',0.95)
confint(ols, 'log(lotsize)',0.99)
confint(ols, 'log(lotsize)',0.90)


# H0: beta_1 = 1
# 1) 추정치와 s.e.로 계산
# (0.54218-1)/0.03265

# 2) 다소 복잡한 방법, theta = beta_1 - 1, and H0: theta = 0
ols11 = lm(I(log(price)-log(lotsize))~log(lotsize),data=Housing)
summary(ols11)


