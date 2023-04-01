library("readxl")
library("dplyr")
library()

dataxls = as_tibble(read_excel("401ksubs.xls", col_names = c("e401k", "inc", "marr",  "male", "age",  "fsize",  "nettfa", "p401k","pira", "incsq", "agesq")))
dataxls

subdata <- dataxls %>% filter(marr==1 & fsize==2)


