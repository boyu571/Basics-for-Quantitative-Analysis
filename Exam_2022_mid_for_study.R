library("readxl")
library("dplyr")

# 1
rental = as_tibble(read.csv("rental.csv"))
rental

# (a)
ols1 = lm(log(rent)~log(pop)+log(avginc)+pctstu, data = rental, year==80|90)
summary(ols1)
# estimated b1 = 0.03%, b2 = 0.87%, city population와 avginc이 1% 증가할때 rent가 각각 0.03%, 0.87% 증가함. 

# (b)
(1-pt(1.158, 124))*2
# p-value는 0.249, 5% 유의수준에서 0가설을 reject하지 못한다.

# (c)
1-pt(1.158, 124)
# p-value는 0.1245, 5% 유의수준에서 0가설을 reject하지 못한다.

# (d)
(1-pt(21.226, 124))*2
# p-value는 0, 1% 유의수준에서 0가설을 reject한다.


#2-(a) True -> 잔차의 합은 항상 0인 이유는 오차평균은 0이라는 가정에 의함
#2-(b) True -> R = 1 - RSS/TSS

  # TSS (Total sum of squares) = RSS (regression sum of squares) + SSE (sum of squared error terms)

# From this point of view, as TSS >= SSE (equality refers to: the deviation in Y is not influenced by the regressors variables in any way) and TSS >= RSS (equality refers to the perfect model). I guess you should be able to understand it mathematically now. If not, feel free to contact me.
# 
# For the intuition. One should understand that is not possible to predict Y better using the regression (beta*X) than predicting Y by reading y1 and stating, "ah y1 is y1." Also, it is not possible to be able to predict less than nothing at all regarding Y with the regressors regarding Y (e.g. The regressors do not correlate with Y in any way.)
# 
# Therefore, 0 =< R <=1. Nevertheless, the boundaries (0 and 1) are in practice rarely obtained. In theory, however, an R of 0 or 1 is possible.

#2-(c) True -> 선형모델의 가정하에 OLS추정기는 BLUE(Best linear unbiased estimator)이며, 분산이 가장작기 때문임
#2-(d) True -> 동일분산 가정에 의함


# 3
discrim = as_tibble(read.csv("Discrim.csv"))
summary(discrim)

# (a)
# 평균 : 11.496, 최소값 : 0, 최대값 : 98.166

# (b)
ols2 = lm(log(psoda)~prpblck+log(income), data = discrim)
summary(ols2)
# income이 1% 증가시 psoda 0.07% 증가. 탄력성

# (c)
ols3 = lm(log(psoda)~prpblck+log(income)+prppov, data = discrim)
summary(ols3)
(1-pt(2.373, 397))*2
# The p-value for testing H0 against the two-sided alternative is about 0.018, so that we reject H0 at the 5% level but not at the 1% level.

# (d)
confint(ols3, 'prpblck',0.99)
# 0이 신뢰 구간 [-6.589484e-05, 0.00152204] 사이에 있으므로 기각할 수 없으며 1% 유의수준에서 통계적으로 유의하지 않음
qt(0.995, 397)

# (e)
ols4 = lm(log(psoda)~prpblck+log(income)+prppov+log(hseval), data = discrim)
summary(ols4)
# log(income) and prppov individually insignificant at even the 15% significance level).

# (f)
# H0: beta2=beta3=0

model1 = lm(log(psoda)~prpblck+log(income)+prppov+log(hseval), data = discrim)
model2 = lm(log(psoda)~prpblck+log(hseval), data = discrim)

n = nrow(discrim) #sample size
uhat1 = resid(model1)
uhat2 = resid(model2)
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/2)/(ssr1/(n-5)) # 제약은 2개
Fstat
pval = 1 - pf(Fstat,2,n-5)
pval
# 0.030

anova(model2, model1)
# They are jointly significant at the 5% level because the outcome of the Fstatistic is about 3.52 with p-value = 0.030. 각각이 insignificant하지만  jointly significant 하다는 것은 서로 상관성이 높기 때문(highly correlated)에 발생.

lht(model1, c("prppov=0","log(income)=0"))


# (g)
# log(hseval) is individually significant, and log(income) and prppov are jointly significant, model e seems the most reliable. If proportion of blacks increases by 0.10, psoda is estimated to increase by 1%, other factors held fixed.

# (h)
# I agree.


