library("readxl")
library("dplyr")

# 1
rdchem = as_tibble(read.csv("rdchem.csv"))
rdchem

# (a)
ols1 = lm(log(rd)~log(sales)+profmarg, data = rdchem)
summary(ols1)

#(b)
ols2 = lm(I(log(rd)-log(sales))~log(sales)+profmarg, data = rdchem)
summary(ols2)

#(c)
ols1 = lm(log(rd)~log(sales)+profmarg, data = rdchem)
summary(ols1)

0.101/2


# 4
mlbdata = as_tibble(read.csv("mlbdata.csv"))
mlbdata

summary(mlbdata)

ols3 = lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, data = mlbdata)
summary(ols3)
confint(ols3, 'years',0.99)

model1 = lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr, data = mlbdata)
model2 = lm(log(salary)~years+gamesyr, data = mlbdata)

anova(model2, model1)

ols4 = lm(log(salary)~years+gamesyr+bavg+hrunsyr, data = mlbdata)
summary(ols4)
0.035943-0.01077
