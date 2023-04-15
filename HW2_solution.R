# Homework2

setwd("D:/2Teaching/0BasicQuant/HW")
dir()

install.packages("readxl")
library("readxl")

# import data from a xls file without header
dataxls = as.matrix(read_excel("401ksubs.xls", col_names = FALSE ))

head(dataxls)
tail(dataxls)


complete.cases(dataxls)
#na.omit returns the object with incomplete cases removed.
data = na.omit(dataxls)
n    = nrow(data)


nettfa = matrix(data[,7])
inc = matrix(data[,2])
age = matrix(data[,5])
marr = matrix(data[,3])
fsize = matrix(data[,6])

# 1) 
sum(subset= marr == 1 & fsize==2)

n = sum(subset= marr == 1 & fsize==2)


#2)
ols1 = lm(nettfa~inc+age, subset= marr == 1 & fsize==2)
summary(ols1)

# age ���̰� �����Ǿ��� ��(age�� ���� ���), inc 1������ ������ �� nettfa�� 1.3 ���� (inc�� nettfa ��� 1000$ �����̹Ƿ� �ҵ��� 1000�޷� ������ �� �������ڻ��� 1300�޷� ����)
# inc�� �����Ǿ��� ��, age ���̰� �� �� �� ������ �� nettfa�� 1.66 (1660�޷�) ����
# not surprising

# 3) p-value�� 0�� �����Ƿ� 1% ���Ǽ��ؿ����� significant

confint(ols1,level=0.95)

?confint  
# 99% confidence interval
# confint(ols1,level=0.99)


# 4) age�� inc�� zero�� ��, nettfa�� -104��� ��. age�� 0�� ����� ���ÿ� ���� ������ ���ǹ���. 

# 5)
tstat = (1.66470-1)/0.16843
tstat 

?pt  #cumulative distribution function of t-distribution

# beta2�� 1���� ũ�ų� �Ǵ� ������ reject�ϹǷ� two-sided test�� p-value�� ������ ���ؼ� ���
2*(1-pt(tstat,n-3))

# 0.01���� p-value�� �����Ƿ� 1% ���Ǽ��ؿ��� null hypothesis�� reject


# 6) beta2�� 1���� Ŀ������ reject�ϹǷ�, p-value�� ���Ϸ��� �����ʸ� ��� 

1-pt(tstat,n-3)
# ������ 0.01���� p-value�� �����Ƿ� 1% ���Ǽ��ؿ��� null hypothesis�� reject 