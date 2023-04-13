library("readxl")
library("dplyr")

# 1
rental = as_tibble(read.csv("rental.csv"))
rental

# (a)
ols1 = lm(log(rent)~log(pop)+log(avginc)+pctstu, data = rental, year==80|90)
summary(ols1)
# estimated b1 = 0.03%, b2 = 0.87%, city population�� avginc�� 1% �����Ҷ� rent�� ���� 0.03%, 0.87% ������. 

# (b)
(1-pt(1.158, 124))*2
# p-value�� 0.249, 5% ���Ǽ��ؿ��� 0������ reject���� ���Ѵ�.

# (c)
1-pt(1.158, 124)
# p-value�� 0.1245, 5% ���Ǽ��ؿ��� 0������ reject���� ���Ѵ�.

# (d)
(1-pt(21.226, 124))*2
# p-value�� 0, 1% ���Ǽ��ؿ��� 0������ reject�Ѵ�.


#2-(a) True -> ������ ���� �׻� 0�� ������ ��������� 0�̶�� ������ ����
#2-(b) True -> ��������� 0�̶�� ������ ����, ��������� 0�̶�� �����Ͽ� �������� ���� ����� 0�� ����� ���� R������ 0~1������ ���� ����
#2-(c) True -> �������� �����Ͽ� OLS������� BLUE(Best linear unbiased estimator)�̸�, �л��� �����۱� ������
#2-(d) True -> ���Ϻл� ������ ����


# 3
discrim = as_tibble(read.csv("Discrim.csv"))
summary(discrim)

# (a)
# ��� : 11.496, �ּҰ� : 0, �ִ밪 : 98.166

# (b)
ols2 = lm(log(psoda)~prpblck+log(income), data = discrim)
summary(ols2)
# income�� 1% ������ psoda 0.07% ����. ź�¼���

# (c)
ols3 = lm(log(psoda)~prpblck+log(income)+prppov, data = discrim)
summary(ols3)
(1-pt(2.373, 397))*2
# The p-value for testing H0 against the two-sided alternative is about 0.018, so that we reject H0 at the 5% level but not at the 1% level.

# (d)
confint(ols3, 'prpblck',0.99)
# 0�� �ŷ� ���� [-6.589484e-05, 0.00152204] ���̿� �����Ƿ� �Ⱒ�� �� ������ 1% ���Ǽ��ؿ��� ��������� �������� ����
qt(0.995, 397)

# (e)
ols4 = lm(log(psoda)~prpblck+log(income)+prppov+log(hseval), data = discrim)
summary(ols4)
# log(income) and prppov individually insignificant(at even the 15% significance level).

# (f)
# H0: beta2=beta3=0

model1 = lm(log(psoda)~prpblck+log(income)+prppov+log(hseval), data = discrim)
model2 = lm(log(psoda)~prpblck+log(hseval), data = discrim)

n = nrow(discrim) #sample size
uhat1 = resid(model1)
uhat2 = resid(model2)
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/2)/(ssr1/(n-5)) # ������ 2��
Fstat
pval = 1 - pf(Fstat,2,n-5)
pval
# 0.030

anova(model2, model1)
# They are jointly significant at the 5% level because the outcome of the Fstatistic is about 3.52 with p-value = 0.030. ������ insignificant������  jointly significant �ϴٴ� ���� ���� ������� ���� ����(highly correlated)�� �߻�.


# (g)
# log(hseval) is individually significant, and log(income) and prppov are jointly significant, model e seems the most reliable. If proportion of blacks increases by 0.10, psoda is estimated to increase by 1%, other factors held fixed.

# (h)
# I agree.

