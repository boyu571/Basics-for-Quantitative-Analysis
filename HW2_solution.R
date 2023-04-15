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

# age 나이가 고정되었을 때(age가 같을 경우), inc 1단위가 증가할 때 nettfa이 1.3 증가 (inc과 nettfa 모두 1000$ 기준이므로 소득이 1000달러 증가할 때 순금융자산이 1300달러 증가)
# inc이 고정되었을 때, age 나이가 한 살 더 많아질 때 nettfa이 1.66 (1660달러) 증가
# not surprising

# 3) p-value가 0에 가까우므로 1% 유의수준에서도 significant

confint(ols1,level=0.95)

?confint  
# 99% confidence interval
# confint(ols1,level=0.99)


# 4) age와 inc이 zero일 때, nettfa이 -104라는 뜻. age가 0인 사람은 샘플에 없기 때문에 무의미함. 

# 5)
tstat = (1.66470-1)/0.16843
tstat 

?pt  #cumulative distribution function of t-distribution

# beta2가 1보다 크거나 또는 작으면 reject하므로 two-sided test의 p-value는 양쪽을 더해서 계산
2*(1-pt(tstat,n-3))

# 0.01보다 p-value가 작으므로 1% 유의수준에서 null hypothesis를 reject


# 6) beta2가 1보다 커야지만 reject하므로, p-value를 구하려면 오른쪽만 계산 

1-pt(tstat,n-3)
# 여전히 0.01보다 p-value가 작으므로 1% 유의수준에서 null hypothesis를 reject 