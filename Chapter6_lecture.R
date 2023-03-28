
#####################
# Chapter 6
#####################

install.packages("Ecdat")
library("Ecdat")

data(Housing, package="Ecdat")

ols = lm(log(price)~log(lotsize),data=Housing)
summary(ols)


# critical value 1% ���Ǽ��� 
qt(0.995,(546-2))
# p-value for two-sided test
2*(1-pt(16.61,546-2))

# confidence interval
confint(ols)
confint(ols, 'log(lotsize)',0.95)
confint(ols, 'log(lotsize)',0.99)
confint(ols, 'log(lotsize)',0.90)


# H0: beta_1 = 1
# 1) ����ġ�� s.e.�� ���
# (0.54218-1)/0.03265

# 2) �ټ� ������ ���, theta = beta_1 - 1, and H0: theta = 0
ols11 = lm(I(log(price)-log(lotsize))~log(lotsize),data=Housing)
summary(ols11)



#######
# ���� 6.1
#repos <- "http://econ.korea.ac.kr/~chirokhan/local-cran"
#install.packages("loedata", repos = repos)
library(loedata)
data(Hcons, package = "loedata")


summary(Hcons)
# age: ������ ����, comm: �ѼҺ����� �� ��ź� ����, rec: ����/��ȭ�� ���� ���� 

# Exercise
# Run the regression of comm on age
# significant?
# coefficient interpretation
# 95% confidence interval



######
# ���� 6.2

data(Klosa, package = "loedata")

summary(Klosa)

#satisfy5: ���� ������ (100�� ����)
#married: ����ڰ� ������ 1 
#working: ����� ������ 1 
#hlth3: �ǰ� ���� ��� 0, ������ 1, ���ڸ� -1

# Run the regression of satisfy5 on married, ��������̸鼭 65�� �̻��� ��츸 


ols2 =
summary(ols2)



# Run the regression of satisfy5 on married,�������, 65�� �̻�, �ǰ� ���°� ��� �̻��� ��츸 
ols3 = 
summary(ols3)


# Run the regression of satisfy5 on married, �������, 65�� �̻�, �ǰ� ���°� ���� ��츸

ols4 = lm
summary(ols4)


# Run the regression of satisfy5 on married and hlth3, �������, 65�� �̻��� ��츸 
ols5 = lm(satisfy5~married+hlth3, data=klosa, subset=working==0 & age>=65)
summary(ols5)


######
# ���� 6.3

data(Ksalary, package = "loedata")
summary(Ksalary)

# avgsal: ��� ���� �޿�
# sales: �����
# emp: ��������
# kospi=1�� �������� ��Ÿ��
# sector: ���� 

# Run the regression of log(avgsal) on log(������ 1�δ� �����), subset: kospi ����, sector�� ElecElectron)

ols6 = 
summary(ols6)

# 95% confidence interval�� ���Ͻÿ�


######
### ����6.5
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