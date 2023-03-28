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

#co2pc: 1�δ� �̻�ȭź�� ���ⷮ(��)
#gdpcppp: 1�δ� GDP(�޷�), PPP(���ŷ�) ����

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
#servpc: 1õ���� ���������� 
#finind: 2010�� �����ڸ���

plot(finind~servpc, data=Pubserv, subset=servpc<28)

serv1 = Pubserv[Pubserv$servpc<28,]
serv2 = subset(Pubserv,servpc<28)  #������ ���


nrow(serv1)
plot(log(finind)~log(servpc), data=serv1)


# Regress log(finind) on log(servpc) (use serv1)
# ols2 = 
# summary(ols2)
# regression line�� �߰��Ͻÿ� 
ols2 = lm(log(finind)~log(servpc), data=serv1)
summary(ols2)

plot(log(finind)~log(servpc), data=serv1)
abline(ols2)

###############
data(Klips)

Klips2 <- subset(Klips, regular==1 & married==1)    # ������ and ��ȥ�� 
nrow(Klips2)
summary(Klips2)

# �뵿�ҵ� labinc, �������� educ

par(mfrow = c(2,1)) # �׸� ������
plot(labinc~educ, data=Klips2)
plot(log(labinc)~educ, data=Klips2)
abline(ols3)


plot(log(labinc)~educ, data=Klips2)
plot(labinc~educ, data=Klips2, log='y')  #�ҵ濡 log�� ���ϰ� ��ǥ�࿡�� ���� �ҵ� �� ��ü�� ǥ�� 


plot(labinc~educ, data=Klips2)
plot(log(labinc)~educ, data=Klips2)
Klips2$mloginc <- with(Klips2, ave(log(labinc), educ, FUN=mean))
points(mloginc~educ, data=Klips2, pch=19, cex=2)  #log(labin)�� ��հ��� ���� ������ ǥ�� 


# ������ and ��ȥ�ڸ� �����Ͽ�, regress log(labinc) on educ
# ols3 = 
# summary(ols3)
# regression line�� �߰��Ͻÿ�

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


# 90�� �ڷḸ �̿��Ͽ� ols4�� ������ ������ �����Ͻÿ�
#ols5 
# summary(ols5)
ols5 = lm(log(sales)~log(price),data=Cigar,subset=year==90)
summary(ols5)


# ���������� ��ȭ
# Regress log(sales) on log(100*price), 90�� �ڷḸ �̿�. ols5 ����� ���Ͻÿ� 
# ols6 = 
# summary(ols6)
ols6 = lm(log(sales)~log(100*price),data=Cigar,subset=year==90)
summary(ols6)


attach(Cigar)

tprice = 10*price

# ���� �� ���� ������ ��������� ���Ͻÿ�. 90�� �ڷḸ �̿�
# ����1: Regression of log(sales) on price
# ����2: Regression of log(sales) on tprice
ols7 = lm(log(sales)~price,data=Cigar,subset=year==90)
summary(ols7)

ols8 = lm(log(sales)~tprice,data=Cigar,subset=year==90)
summary(ols8)

detach(Cigar)

########

data(Consumption,package="Ecdat")
colnames(Consumption)

# yd ĳ���� ���ΰ�ó�мҵ�, 1986�� �Һ�����
# ce ������ ���μҺ�����, 1986�� �Һ�����

lm(ce~yd,data=Consumption)

par(mfrow = c(1,1))

plot(ce~yd,data=Consumption)

nrow(Consumption)
plot(ce~seq(1:200),data=Consumption)
plot(yd~seq(1:200),data=Consumption)

# �̷� �����ʹ� ��������

########
curve(dnorm(x,mean=0,sd=1),-6,6)




qnorm(0.01)
?qt





