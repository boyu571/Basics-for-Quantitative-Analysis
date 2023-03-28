##### R basic 3 #####

install.packages("data.table")

library(data.table)
setwd("C:/Users/boyu5/Desktop/23�� ���б�/��Ʈ�Թ�(������)/R codes/R_basics")
dir()


## ----fread_select, �����ؼ� �ҷ����̱�-------------------------------------

survey <- read.csv("old_survey.csv")
attach(survey)   # R�� ����(?)
survey

survey = cbind(handedness, height, handspan)

## ----cor: correlation, use = "complete.obs"�� ��� missing observations ���� -----------
cor(handspan, height, use = "complete.obs")

## ----cor_dt--------------------------------------------------------------
cor(survey, use = "complete.obs")
#alternatively, there's the na.omit function. na.omit�� NA�� ����, ������ ��� 
cor(na.omit(survey))

# same correlation as in line 15?

## ----find_missing--------------------------------------------------------
which(is.na(handedness) & !is.na(height) & !is.na(handspan))

## ----cov: covariance ------------------------------------------------------
cov(survey, use = "complete.obs")
cov(na.omit(survey))


#####################
## function 
#####################

## ----function_write--------------------------------------------------- # ������ ǥ��ȭ �۾�standardizing
z.score = function(x){
  z = (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(z)
}

## ----mymean--------------------------------------------------------------
mymean = function(x){
  x = x[!is.na(x)]    # x is overwritten
  x.bar = sum(x)/length(x)
  return(x.bar)
}

## ----mean: R�� ����� �Լ�----------------------------------
mean(height, na.rm = TRUE)

## ----mymean: ������ ���ǵ� �Լ�-----------------------------------------
mymean(height)

## ----mymean2-------------------------------------------------------------
mymean2 = function(x){
  x.bar = sum(x, na.rm = TRUE)/length(x)
  return(x.bar)
}
mymean2(height)
# mymean�� mymean2�� ��
# ���� �ٸ� ������ length(x)���� mymean�� ����ġ ���ŵ� ���� ���� �ٸ��� ����
# mymean2 �Լ� �ȿ��� sum ignores missing observations but length does not.
 

## ----myvar---------------------------------------------------------------
myvar = function(x){
  x = x[!is.na(x)]
  s.squared = sum((x-mymean(x))^2)/(length(x) - 1)
  return(s.squared)
}

## ----myvar_test----------------------------------------------------------
var(handspan, na.rm = TRUE)
myvar(handspan)

## ----exercise_2----------------------------------------------------------
# skewness ���� �ݵ�� �˻��ؼ� ���� �ʼ�(���� �� ��Ÿ���� ��Ľ�)

#Exercise #2 - Write a Function to Calculate Skewness
skew = function(x){
  x = x[!is.na(x)]
  numerator = sum((x - mean(x))^3)/length(x)
  denominator = sd(x)^3
  return(numerator/denominator)  
}
skew(handedness)

## ----function_return_data.table------------------------------------------
summary.stats = function(x){
  x = x[!is.na(x)]
  sample.mean = mean(x)
  std.dev  = sd(x)
  out = data.table(sample.mean, std.dev)
  return(out)
}
results = summary.stats(handedness)
results
results$sample.mean
results$std.dev

## ----mycov---------------------------------------------------------------
mycov = function(x, y){
  
  keep = !is.na(x) & !is.na(y) # �Ѵ� ����ġ�� ����
  x = x[keep]
  y = y[keep]
  
  n = length(x)
  
  s.xy = sum( (x - mean(x)) * (y - mean(y)) ) / (n-1) # n-1�� ������ ��� �� �� ������ 11�� ����� ����
  return(s.xy)
}

## ----mycov �Լ��� R�� ����� �Լ� ��-----------------------------------------
cov(handspan, handedness, use = "complete.obs")
mycov(handspan, handedness)