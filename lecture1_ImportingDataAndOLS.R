##### Importing data and OLS estimation #####

# change directory and import data
# getwd()
# setwd("C:/Users/Heejoon/Desktop/R_Quant")
# dir()


## Load data

# Importing a csv file


# 다양한 방법이 가능
data1 <- read.csv("DataHousingPrice.csv")

# Regress housing price on number of bedrooms
ols = lm(PRICE~BDRMS,data=data1)
summary(ols)

attach(data1)   # R에 장착
# detach(data1)
Y <- cbind(PRICE)
X <- cbind(LOTSIZE, SQRFT, BDRMS)

# Orternatively, matrix로 변경
data = as.matrix(read.csv("DataHousingPrice.csv",header=T))

y = cbind(data[,1])   # or y = matrix(data[,1])
x = cbind(data[,2:4])
head(x)


# import txt file
data22 = as.matrix(read.table("DataHousingPrice.txt",header=T))


#### import excel data

# Install package : readxl
install.packages("readxl")
# Use installed package
library("readxl")

# import data from a xls file
dataxls = as.matrix(read_excel("DataHousingPrice.xls"))

# import data in the first sheet in a xlsx file
dataxlsx = as.matrix(read_excel("DataHousingPrice.xlsx",sheet=1)) 
?read_excel

# Exercise
# import data in the second sheet in a xlsx file



## create new dataset without missing data if there are missing data

#Case1: no missing data

# NA가 있는지 확인
complete.cases(data)
# na.omit는 NA(missing data)를 제거
data = na.omit(data)  
n    = nrow(data)


# Case2: there are missing data
dataMissing = as.matrix(read.csv("DataHousingPriceMISSING.csv",header=T))

complete.cases(dataMissing)
dataCleaned = na.omit(dataMissing)
nCleaned    = nrow(dataCleaned)


#################
## OLS estimation


# Regress housing price on number of bedrooms

PRICE = data[,1]
BDRMS = data[,4]

fit0 = lm(PRICE~BDRMS)
summary(fit0)

plot(PRICE ~ BDRMS)
abline(fit0) # regression line
# alternatively
abline(lm(PRICE~BDRMS))

## ----abline과 sample mean 추가 ---------------------------------
plot(PRICE ~ BDRMS)
abline(fit0)
abline(v = mean(BDRMS, na.rm = TRUE),
       h = mean(PRICE, na.rm = TRUE),
       col = 'red', lty = 2)


# Regress PRICE on SQRFT and BDRMS
LOTSIZE = data[,2]
SQRFT = data[,3]

Y = PRICE
X = cbind(SQRFT, BDRMS)

fit1 = lm(Y ~ X)
summary(fit1)

# Regress log(PRICE) on log(LOTSIZE)
fit2 = lm(log(PRICE)~log(LOTSIZE))
summary(fit2)

# Regress log(PRICE) on log(LOTSIZE), log(SQRFT)
fit3 = lm(log(PRICE)~log(LOTSIZE)+log(SQRFT))
fit3

# Exercise
# Regress log(PRICE) on log(LOTSIZE), log(SQRFT) and Bedrooms
fit4 = lm(log(PRICE)~log(LOTSIZE)+log(SQRFT)+BDRMS)
fit4
summary(fit4)






####################################
# OLS estimation using matrix
####################################

n    = nrow(data)
y = matrix(data[,1])
x = cbind(1,data[,2:4])
head(x)

invx = solve(t(x)%*%x)
olsb = invx%*%t(x)%*%y
olsb

#fit2 = lm(y~x-1) # regression without intercept
#summary(fit2)
