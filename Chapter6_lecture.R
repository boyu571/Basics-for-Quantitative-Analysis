
#####################
# Chapter 6
#####################

install.packages("Ecdat")
library("Ecdat")

data(Housing, package="Ecdat")

ols = lm(log(price)~log(lotsize),data=Housing)
summary(ols)


# critical value 1% 유의수준 
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



#######
# 예제 6.1
#repos <- "http://econ.korea.ac.kr/~chirokhan/local-cran"
#install.packages("loedata", repos = repos)
library(loedata)
data(Hcons, package = "loedata")


summary(Hcons)
# age: 가구주 나이, comm: 총소비지출 중 통신비 비중, rec: 오락/문화비 지출 비중 

# Exercise
# Run the regression of comm on age
# significant?
# coefficient interpretation
# 95% confidence interval



######
# 예제 6.2

data(Klosa, package = "loedata")

summary(Klosa)

#satisfy5: 삶의 만족도 (100점 만점)
#married: 배우자가 있으면 1 
#working: 취업해 있으면 1 
#hlth3: 건강 정도 평균 0, 좋으면 1, 나쁘면 -1

# Run the regression of satisfy5 on married, 비취업자이면서 65세 이상인 경우만 


ols2 =
summary(ols2)



# Run the regression of satisfy5 on married,비취업자, 65세 이상, 건강 상태가 평균 이상인 경우만 
ols3 = 
summary(ols3)


# Run the regression of satisfy5 on married, 비취업자, 65세 이상, 건강 상태가 나쁜 경우만

ols4 = lm
summary(ols4)


# Run the regression of satisfy5 on married and hlth3, 비취업자, 65세 이상인 경우만 
ols5 = lm(satisfy5~married+hlth3, data=klosa, subset=working==0 & age>=65)
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

ols6 = 
summary(ols6)

# 95% confidence interval을 구하시오


######
### 예제6.5
install.packages("AER")
library("AER")

data(CigarettesB, package = "AER")
summary(CigarettesB)

ols7 = lm(packs~price,data=CigarettesB)
summary(ols7)

# test wheter price elasticity is -1
# theta = beta_1 +1 (so that theta=0 under Ho)


ols8 = 
summary(ols8)
