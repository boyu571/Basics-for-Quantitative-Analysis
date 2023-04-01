
#####################
# Chapter 6
#####################

install.packages("Ecdat")
library("Ecdat")

data(Housing, package="Ecdat")

ols = lm(log(price)~log(lotsize),data=Housing)
summary(ols)
# ols = lm(log(price)~log(lotsize)+bedrooms,data=Housing)
# summary(ols)



# critical value 1% 유의수준 
qt(0.995,(546-2))
# p-value for two-sided test
2*(1-pt(16.61,546-2))
# t-value 16.61

# confidence interval
confint(ols)
confint(ols, 'log(lotsize)',0.95)
confint(ols, 'log(lotsize)',0.99)
confint(ols, 'log(lotsize)',0.90)


# H0: beta_1 = 1
# 1) 추정치와 s.e.로 계산
tstat = (0.54218-1)/0.03265

pt(-14.02205,(546-2)) *2

# 2) 다소 복잡한 방법, theta = beta_1 - 1, and H0: theta = 0
ols11 = lm(I(log(price)-log(lotsize))~log(lotsize),data=Housing)
summary(ols11)
#I 새로운 종속변수


#######
# 예제 6.1
repos <- "http://econ.korea.ac.kr/~chirokhan/local-cran"
install.packages("loedata", repos = repos)
library(loedata)
data(Hcons, package = "loedata")


summary(Hcons)
# age: 가구주 나이, comm: 총소비지출 중 통신비 비중, rec: 오락/문화비 지출 비중 

# Exercise
# Run the regression of comm on age
# significant?
# coefficient interpretation
# 95% confidence interval

ols12 = lm(comm~age,data=Hcons)
summary(ols12)
# 한살 증가할 수록 0.01퍼센트포인트 증가,
# 마지막이 p-value0.0335라서 5%에 미치지 못함

confint(ols12, 'age', 0.95)
#0을 포함하고 있지 않음

######
# 예제 6.2

data(Klosa, package = "loedata")

summary(Klosa)

#satisfy5: 삶의 만족도 (100점 만점)
#married: 배우자가 있으면 1 
#working: 취업해 있으면 1 
#hlth3: 건강 정도 평균 0, 좋으면 1, 나쁘면 -1

# Run the regression of satisfy5 on married, 비취업자이면서 65세 이상인 경우만 


ols2 = lm(satisfy5~married, data=Klosa, subset=working==0 & age>=65)
summary(ols2)



# Run the regression of satisfy5 on married,비취업자, 65세 이상, 건강 상태가 평균 이상인 경우만 
ols3 = lm(satisfy5~married, data=Klosa, subset=working==0 & age>=65 & hlth3 >=0)
summary(ols3)
# p-value가 0.05보다 높아서 기각을 못함
# 두개의 상관관계가 없다는 의미


# Run the regression of satisfy5 on married, 비취업자, 65세 이상, 건강 상태가 나쁜 경우만

ols4 = lm(satisfy5~married, data=Klosa, subset=working==0 & age>=65 & hlth3 ==-1)
summary(ols4)


# Run the regression of satisfy5 on married and hlth3, 비취업자, 65세 이상인 경우만 
ols5 = lm(satisfy5~married+hlth3, data=Klosa, subset=working==0 & age>=65)
summary(ols5)


######
# 예제 6.3

data(Ksalary, package = "loedata")
summary(Ksalary)

# avgsal: 평균 연간 급여
# sales: 매출액
# emp: 종업원수
# kospi=1은 상장기업을 나타냄
# sector: 업종 

# Run the regression of log(avgsal) on log(종업원 1인당 매출액), subset: kospi 상장, sector는 ElecElectron)

ols6 = lm(log(avgsal)~log(sales/emp), data=Ksalary, subset=kospi==1 & sector=="ElecElectron")
summary(ols6)

# 95% confidence interval을 구하시오
confint(ols6)

######
### 예제6.5
install.packages("AER")
library("AER")

data(CigarettesB, package = "AER")
summary(CigarettesB)

ols7 = lm(packs~price,data=CigarettesB)
summary(ols7)

# test whether price elasticity is -1
# theta = beta_1 +1 (so that theta=0 under Ho)
tstat1 = (-1.1983+1)/0.2818
tstat1

ols8 = lm(I(packs+price)~price, data=CigarettesB)
summary(ols8)
# Pvalue가 0.05보다 크기 때문에 price elasticity가 -1임을 reject하지 못함
