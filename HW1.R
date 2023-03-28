install.packages("readxl")
library("readxl")
library("dplyr")



#문제 1
dataxls01 = as_tibble(read_excel("CEOSAL2.xls", col_names = c("salary", "age", "college",  "grad", "comten",  "ceoten",  "sales", "profits","mktval", "lsalary", "lsales", "lmktval", "comtensq",  "ceotensq", "profmarg")))
summary(dataxls01)

# 1-1
mean(dataxls01$salary)
mean(dataxls01$ceoten)

# 1-2
dataxls01 %>% count(ceoten==0)
max(dataxls01$ceoten)

# 1-3
fit1 = lm(log(salary) ~ ceoten, data=dataxls01)
summary(fit1)



#문제 2
dataxls02 = as_tibble(read_excel("401k.xls", col_names = c("prate", "mrate", "totpart",  "totelg", "age",  "totemp",  "sole", "ltotemp")))

summary(dataxls02)

# 2-1
mean(dataxls02$prate)
mean(dataxls02$mrate)

# 2-2
fit2 = lm(prate ~ mrate, data=dataxls02)
summary(fit2)



