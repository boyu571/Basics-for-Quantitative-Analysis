# Partial out


setwd("D:/2Teaching/0BasicQuant/R_Quant_updated")
dir()

install.packages("readxl")
# Use installed package
library("readxl")

# import data from a xls file WITHOUT header

dataxls = as.matrix(read_excel("401k.xls", col_names = FALSE ))
?read_excel

head(dataxls)
tail(dataxls)


complete.cases(dataxls)
#na.omit returns the object with incomplete cases removed.
data = na.omit(dataxls)
n    = nrow(data)


prate = matrix(data[,1])
mrate = matrix(data[,2])
age = matrix(data[,5])


fit = lm(prate~mrate+age)
summary(fit)


fit2 = lm(mrate~age)
uhat2 = resid(fit2)   
# 잔차 : age가 mrate에 영향을 미치는 부분을 제거한 부분

fit3 = lm(prate~uhat2+age)
summary(fit3)


fit4 = lm(prate~age)
uhat4 = resid(fit4)


fit5 = lm(uhat4~uhat2)
summary(fit5)


### Example of perfect collinearity

sole = matrix(data[,7])
head(sole)

notsole = 1-sole
data2 = cbind(sole, notsole)
head(data2)

fit6 = lm(prate~sole)
coeftest(fit6)

fit7 = lm(prate~sole+notsole)
coeftest(fit7)

