# Chapter 10

#예제 10.1
data(Regko,package="loedata")

#우리나라 지역별(시,군,구) 자료들 2014-2016년 평균값 #divorce: 1천명당 이혼건수
#regpop: 주민등록 인구
#drink: 음주율, 음주인구 비율 %
#hdrink: 고위험음주율, 음주자 중 고위험 음주자 비율 %
#smoke: 흡연율, 흡연인구 비율 %
#aged: 고령인구비율 %
#type: 지형 유형(1: 광역시를 제외한 시, 2: 군, 3: 광역시의 구)
#grdp: 지역별 gdp


#분석대상을 '군'지역 (type=2)로 국한해서 분석
#Regress log(divorce) on log(regpop), log(drink), log(hdrink), log(smoke), log(1인당 지역별 gdp), log(aged)

ols = lm(log(divorce)~log(regpop)+log(drink)+log(hdrink)+log(smoke),data=Regko,type==2)
summary(ols)


#Regress log(divorce) on log(regpop), log(drink), log(hdrink), log(smoke), log(1인당 지역별 gdp), log(aged)
ols1 = lm(log(divorce)~log(regpop)+log(drink)+log(hdrink)+log(smoke)+log(grdp/regpop)+log(aged),data=Regko,type==2)
summary(ols1)

#계수 추정치 해석
#individual significance



# 예제 10.2
install.packages("wooldridge")
library(wooldridge)

data(twoyear, package = "wooldridge")
nrow(twoyear)
names(twoyear)


#lwage: log(wage)
#jc: 2년제 대학을 다닌 햇수
#univ: 4년제 대학을 다닌 햇수
#exper: 경력

ols2 = lm(lwage~jc+univ+exper,data = twoyear)
summary(ols2)

#jc와 univ 계수 해석

#H0: beta1=beta2, H1: not equal


#theta = beta1-beta2  (theta=0 under H0)
twoyear$totcoll <= with(twoyear, jc+univ)
# 이렇게 하자...
ols3 = lm(lwage~jc+I(jc+univ)+exper, data=twoyear)
summary(ols3)

# jc의 계수 추정치? theta -0.01
# significant? no....can't reject


# 예제 10.3

#H0: beta1=beta2, H1: not equal
#same H0 and H1, F-test

#unrestricted model
model1 = lm(lwage~jc+univ+exper,data=twoyear) 
uhat1 = resid(model1)

#restricted model
model2 = lm(lwage~I(jc+univ)+exper,data=twoyear)
uhat2 = resid(model2)

n = nrow(twoyear) #sample size
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/1)/(ssr1/(n-4)) # 제약은 하나 b1=b2
Fstat
pval = 1 - pf(Fstat,1,n-4)
pval

# same p-value as in 예제 10.2?
# 제약의 개수가 하나인 경우라서 동일하게 나옴


#자동 계산도 가능
#1)
anova(model2,model1)

#2)
install.packages("car")
library("car")

?lht
#lht: test linear hypothesis 
lht(model1,"jc=univ")

#3) Wald test for nested models
#install.packages("lmtest")
#library("lmtest")
#?waldtest
#waldtest(model1, model2)

#model1 = lm(lwage~jc+I(jc+univ)+exper,data=twoyear)
#waldtest(model1, model2)

# 또는
#model1 = lm(lwage~jc+I(jc+univ)+exper,data=twoyear)
#waldtest(model1, "jc")



# 예제 10.3
install.packages("Ecdat")
library("Ecdat")

data(Doctor,package="Ecdat")
summary(Doctor)
#doctor: 의사 방문횟수
#children: 가구 내 어린이의 수
#access: 병원에 대한 접근성의 척도(0~1)
#health: 건강상태의 척도(클수록 건강이 나쁨)


ols4 = lm(doctor~children+access+health,data=Doctor)
summary(ols4)

# F-test
ols5 = lm(doctor~children+health,data=Doctor)
summary(ols5)

n = nrow(Doctor) #sample size
uhat4 = resid(ols4)
uhat5 = resid(ols5)
ssr4 = sum(uhat4^2)  #SSR of unrestricted model
ssr5 = sum(uhat5^2)  #SSR of restricted model 
Fstat = ((ssr5-ssr4)/1)/(ssr4/(n-4)) # 제약은 하나 b1=b2
Fstat
pval = 1 - pf(Fstat,1,n-4)
pval


#Is "access" significant?
#F-test 사용
library(car)
lht(ols4,"access")

# t값과 F값? 


# 예제 10.5
# H0: beta1=beta2=beta3
# H0: beta1=beta2 and beta2=beta3

ols6 = lm(lwage~jc+univ+exper,data = twoyear)
lht(ols6, c("jc=univ","univ=exper"))


# What about H0: beta1=beta2 and beta1=beta3
ols7 = lm(lwage~I(jc+univ+exper), data=twoyear)


n = nrow(twoyear) #sample size
uhat6 = resid(ols6)
uhat7 = resid(ols7)
ssr6 = sum(uhat6^2)  #SSR of unrestricted model
ssr7 = sum(uhat7^2)  #SSR of restricted model 
Fstat = ((ssr7-ssr6)/2)/(ssr6/(n-4)) # 제약은 두개
Fstat
pval = 1 - pf(Fstat,2,n-4)
pval


# female, black 변수 추가 예제

model1 = lm(lwage~jc+univ+exper+female+black,data = twoyear)

# What about H0: beta1=beta2 and beta1=beta3
model2 = lm(lwage~jc+univ, data=twoyear)

n = nrow(twoyear) #sample size
uhat1 = resid(model1)
uhat2 = resid(model2)
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/3)/(ssr1/(n-4)) # 제약은 세개
Fstat
pval = 1 - pf(Fstat,3,n-4)
pval





lht(ols5, c("jc=univ","jc=exper"))



# Using matrix
restriction=rbind(c(0,1,-1,0),c(0,0,1,-1))

lht(ols5,restriction)


# 예제 10.6
# H0: beta1=beta2=beta3
model_re = lm(lwage~I(jc+univ+exper),data=twoyear)
twoyear$resid = model_re$resid
ols6 = lm(resid~jc+univ+exper,data = twoyear)
R2aux = summary(ols6)$r.sq
R2aux
n = nrow(twoyear)
lmstat = n*R2aux
lmstat
qchisq(.95,2)   #critical value
1-pchisq(lmstat,2) #p-value


# jointly significant?
# H0: beta1=beta2=beta3=0

#F-test statistic
#p-value


