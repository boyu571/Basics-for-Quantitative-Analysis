library("readxl")
library("dplyr")

dataxls = as_tibble(read_excel("401ksubs.xls", col_names = c("e401k", "inc", "marr",  "male", "age",  "fsize",  "nettfa", "p401k","pira", "incsq", "agesq")))
dataxls

# 1-1
subdata <- dataxls %>% filter(marr==1 & fsize==2)
count(subdata)

# 1-2
ols = lm(nettfa ~ inc + age, data=subdata)
summary(ols)

# 1-3
confint(ols, 'age', 0.95)
confint(ols, 'inc', 0.95)

# 1-5
ols2 = lm(I(nettfa - age) ~ inc + age, data=subdata)
summary(ols2)

qt(0.995,(1491))
