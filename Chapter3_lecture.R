# Chapter 3

dir()

install.packages("Ecdat")
library("Ecdat")

#install.packages("lmtest")
#install.packages("AER")
#install.packages("sandwich")
#install.packages("car")

#library("lmtest")
#library("AER")
#library("sandwich")
#library("car")


######

repos <- "http://econ.korea.ac.kr/~chirokhan/local-cran"
install.packages("loedata", repos = repos)
library(loedata)

data(Ekc)

names(Ekc)
nrow(Ekc)

head(Ekc)
summary(Ekc)

#co2pc: 1인당 이산화탄소 배출량(톤)
#gdpcppp: 1인당 GDP(달러), PPP(구매력) 기준

plot(co2pc ~ gdppcppp, data=Ekc)
plot(co2pc ~ log(gdppcppp), data=Ekc)


plot(co2pc ~ log(gdppcppp))  #Why?


attach(Ekc)
plot(co2pc ~ log(gdppcppp))

detach(Ekc)
plot(co2pc ~ log(gdppcppp))


ols1 = lm(co2pc ~ log(gdppcppp), data=Ekc)
summary(ols1)

plot(co2pc ~ log(gdppcppp), data=Ekc)
abline(ols1)

###############
data(Pubserv)
#data(Pubserv, package = "loedata")
head(Pubserv)
nrow(Pubserv)

summary(Pubserv)
#servpc: 1천명당 공무원비율 
#finind: 2010년 재정자립도

plot(finind~servpc, data=Pubserv, subset=servpc<28)

serv1 = Pubserv[Pubserv$servpc<28,]
serv2 = subset(Pubserv,servpc<28)  #동일한 결과


nrow(serv1)
plot(log(finind)~log(servpc), data=serv1)


# Regress log(finind) on log(servpc) (use serv1)
# ols2 = 
# summary(ols2)
# regression line을 추가하시오 
ols2 = lm(log(finind)~log(servpc), data=serv1)
summary(ols2)

plot(log(finind)~log(servpc), data=serv1)
abline(ols2)

###############
data(Klips)

Klips2 <- subset(Klips, regular==1 & married==1)    # 정규직 and 기혼자 
nrow(Klips2)
summary(Klips2)

# 노동소득 labinc, 교육수준 educ

par(mfrow = c(2,1)) # 그림 여러개
plot(labinc~educ, data=Klips2)
plot(log(labinc)~educ, data=Klips2)
abline(ols3)


plot(log(labinc)~educ, data=Klips2)
plot(labinc~educ, data=Klips2, log='y')  #소득에 log를 취하고 좌표축에는 원래 소득 값 자체를 표기 


plot(labinc~educ, data=Klips2)
plot(log(labinc)~educ, data=Klips2)
Klips2$mloginc <- with(Klips2, ave(log(labinc), educ, FUN=mean))
points(mloginc~educ, data=Klips2, pch=19, cex=2)  #log(labin)의 평균값이 굵은 점으로 표시 


# 정규직 and 기혼자만 고려하여, regress log(labinc) on educ
# ols3 = 
# summary(ols3)
# regression line을 추가하시오

ols3 = lm(log(labinc)~educ, data=Klips2)
summary(ols3)

plot(log(labinc)~educ, data=Klips2)
abline(ols3)
  
##################

data(Cigar,package="Ecdat")
names(Cigar)
head(Cigar)

sum(Cigar$year==70)
ols4 = lm(log(sales)~log(price),data=Cigar,subset=year==70)
summary(ols4)


# 90년 자료만 이용하여 ols4와 동일한 모형을 추정하시오
#ols5 
# summary(ols5)
ols5 = lm(log(sales)~log(price),data=Cigar,subset=year==90)
summary(ols5)


# 측정단위의 변화
# Regress log(sales) on log(100*price), 90년 자료만 이용. ols5 결과와 비교하시오 
# ols6 = 
# summary(ols6)
ols6 = lm(log(sales)~log(100*price),data=Cigar,subset=year==90)
summary(ols6)


attach(Cigar)

tprice = 10*price

# 다음 두 선형 모형의 추정결과를 비교하시오. 90년 자료만 이용
# 모형1: Regression of log(sales) on price
# 모형2: Regression of log(sales) on tprice
ols7 = lm(log(sales)~price,data=Cigar,subset=year==90)
summary(ols7)

ols8 = lm(log(sales)~tprice,data=Cigar,subset=year==90)
summary(ols8)

detach(Cigar)

########

data(Consumption,package="Ecdat")
colnames(Consumption)

# yd 캐나다 개인가처분소득, 1986년 불변가격
# ce 개나다 개인소비지출, 1986년 불변가격

lm(ce~yd,data=Consumption)

par(mfrow = c(1,1))

plot(ce~yd,data=Consumption)

nrow(Consumption)
plot(ce~seq(1:200),data=Consumption)
plot(yd~seq(1:200),data=Consumption)

# 이런 데이터는 조심하자

########
curve(dnorm(x,mean=0,sd=1),-6,6)




qnorm(0.01)
?qt






