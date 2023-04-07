# Chapter 10

#���� 10.1
data(Regko,package="loedata")

#�츮���� ������(��,��,��) �ڷ�� 2014-2016�� ��հ� #divorce: 1õ���� ��ȥ�Ǽ�
#regpop: �ֹε�� �α�
#drink: ������, �����α� ���� %
#hdrink: ������������, ������ �� ������ ������ ���� %
#smoke: ������, �����α� ���� %
#aged: �����α����� %
#type: ���� ����(1: �����ø� ������ ��, 2: ��, 3: �������� ��)
#grdp: ������ gdp


#�м������ '��'���� (type=2)�� �����ؼ� �м�
#Regress log(divorce) on log(regpop), log(drink), log(hdrink), log(smoke), log(1�δ� ������ gdp), log(aged)

ols = lm(log(divorce)~log(regpop)+log(drink)+log(hdrink)+log(smoke),data=Regko,type==2)
summary(ols)


#Regress log(divorce) on log(regpop), log(drink), log(hdrink), log(smoke), log(1�δ� ������ gdp), log(aged)
ols1 = lm(log(divorce)~log(regpop)+log(drink)+log(hdrink)+log(smoke)+log(grdp/regpop)+log(aged),data=Regko,type==2)
summary(ols1)

#��� ����ġ �ؼ�
#individual significance



# ���� 10.2
install.packages("wooldridge")
library(wooldridge)

data(twoyear, package = "wooldridge")
nrow(twoyear)
names(twoyear)


#lwage: log(wage)
#jc: 2���� ������ �ٴ� �޼�
#univ: 4���� ������ �ٴ� �޼�
#exper: ���

ols2 = lm(lwage~jc+univ+exper,data = twoyear)
summary(ols2)

#jc�� univ ��� �ؼ�

#H0: beta1=beta2, H1: not equal


#theta = beta1-beta2  (theta=0 under H0)
twoyear$totcoll <= with(twoyear, jc+univ)
# �̷��� ����...
ols3 = lm(lwage~jc+I(jc+univ)+exper, data=twoyear)
summary(ols3)

# jc�� ��� ����ġ? theta -0.01
# significant? no....can't reject


# ���� 10.3

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
Fstat = ((ssr2-ssr1)/1)/(ssr1/(n-4)) # ������ �ϳ� b1=b2
Fstat
pval = 1 - pf(Fstat,1,n-4)
pval

# same p-value as in ���� 10.2?
# ������ ������ �ϳ��� ���� �����ϰ� ���ȿ�


#�ڵ� ��굵 ����
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

# �Ǵ�
#model1 = lm(lwage~jc+I(jc+univ)+exper,data=twoyear)
#waldtest(model1, "jc")



# ���� 10.3
install.packages("Ecdat")
library("Ecdat")

data(Doctor,package="Ecdat")
summary(Doctor)
#doctor: �ǻ� �湮Ƚ��
#children: ���� �� ����� ��
#access: ������ ���� ���ټ��� ô��(0~1)
#health: �ǰ������� ô��(Ŭ���� �ǰ��� ����)


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
Fstat = ((ssr5-ssr4)/1)/(ssr4/(n-4)) # ������ �ϳ� b1=b2
Fstat
pval = 1 - pf(Fstat,1,n-4)
pval


#Is "access" significant?
#F-test ���
library(car)
lht(ols4,"access")

# t���� F��? 


# ���� 10.5
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
Fstat = ((ssr7-ssr6)/2)/(ssr6/(n-4)) # ������ �ΰ�
Fstat
pval = 1 - pf(Fstat,2,n-4)
pval


# female, black ���� �߰� ����

model1 = lm(lwage~jc+univ+exper+female+black,data = twoyear)

# What about H0: beta1=beta2 and beta1=beta3
model2 = lm(lwage~jc+univ, data=twoyear)

n = nrow(twoyear) #sample size
uhat1 = resid(model1)
uhat2 = resid(model2)
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/3)/(ssr1/(n-4)) # ������ ����
Fstat
pval = 1 - pf(Fstat,3,n-4)
pval





lht(ols5, c("jc=univ","jc=exper"))



# Using matrix
restriction=rbind(c(0,1,-1,0),c(0,0,1,-1))

lht(ols5,restriction)


# ���� 10.6
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

