#####################
# Chapter 8 examples
# 예제 8.1, 예제 8.2

repos <- "http://econ.korea.ac.kr/~chirokhan/local-cran"
install.packages("loedata", repos = repos)
library(loedata)

data(Death)

summary(Death)

# 우리나라 군별 사망률 자료
# deathrate: 사망률 인구 1천명당 연간 사망자 수
# drink: 음주율, 백분율

# Run the regression of deathrate on drink, 2010년 자료만 이용 (subset=year==2010)

ols1 = lm(deathrate~drink,data=Death,subset=year==2010)
summary(ols1)
# beta1 estimate is negative?



# aged: 해당 지역 고령자 비율
plot(drink ~ aged,data=Death)


# Run the regression of deathrate on drink and aged, year 2010

ols2 = lm(deathrate~drink+aged, data=Death, subset = year==2010)
summary(ols2)
# beta1 estimate?
# 음수에서 양수로 전환, aged와 deathrate 사이에 음의 상관관계를 제거하고 추정하였더니 양수로 나옴옴


## 분해정리 page 209
## partial out

#install.packages("Ecdat")
library("Ecdat")

data(Housing,package="Ecdat")
summary(Housing)

ols3 = lm(log(price)~log(lotsize)+bedrooms,data=Housing)
coefficients(ols3)

# 1) run the regression of log(lotsize) on bedrooms and get the residual

ols4 = lm(log(lotsize)~bedrooms,data=Housing)
coefficients(ols4)

uhat_x1 = resid(ols4)

# 2) run the regression of log(price) on uhat_x1
# check out the coefficient estimate of uhat_x1

ols5 = lm(log(price)~uhat_x1,data=Housing)
coefficients(ols5)
# 추정치가 동일하게 나옴

ols6 = lm(log(price)~uhat_x1+bedrooms,data=Housing)
coefficients(ols6)

# checking, run the regression of uhat_y on uhat_x1
# run the regression of log(price) on bedrooms and get the residual
ols7 = lm(log(price)~bedrooms,data=Housing)
uhat_y = resid(ols7)

ols8=lm(uhat_y~uhat_x1+bedrooms,data=Housing)
coefficients(ols8)

ols9=lm(uhat_y~uhat_x1,data=Housing)
coefficients(ols9)

# partial out(분해정리)에 대한 내용


######
# 예제 8.3 perfect collinearity

data(Wages1,package="Ecdat")
summary(Wages1)
head(Wages1)
# We make a dummy variable 'male' and 'female'
Wages1$male <- as.numeric(Wages1$sex=="male")
Wages1$female <- as.numeric(Wages1$sex=="female")

head(Wages1)
sum(Wages1$male)
sum(Wages1$female)


lm(wage~female,data=Wages1)

lm(wage~female,data=Wages1,subset=female==1)
# female 변수의 값이 고정되어 있기 때문에  

lm(wage~female+male,data=Wages1)
# perfect collinearity (intercept가 포함되어 있기 때문에)

lm(wage~female+male-1,data=Wages1)
# intercept를 빼고 regression. perfect collinearity???

lm(wage~female,data=Wages1)
# 6.313-5.147=1.166


ols9 = lm(log(wage)~female+school+exper,data=Wages1)
summary(ols9)


# random numbers
Wages1$rnd = rnorm(nrow(Wages1))
head(Wages1)
# 해당 값은 설명력이 전혀 없는 변수임

summary(lm(log(wage)~female+school+exper,data=Wages1))$r.sq
summary(lm(log(wage)~female+school+exper,data=Wages1))$

# R 제곱은 어떤 변수가 포함되어도 증가하는 경향 
summary(lm(log(wage)~female+school+exper+rnd,data=Wages1))$r.sq
summary(lm(log(wage)~female+school+exper+rnd,data=Wages1))
