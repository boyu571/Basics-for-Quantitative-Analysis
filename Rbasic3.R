##### R basic 3 #####

install.packages("data.table")

library(data.table)
setwd("C:/Users/boyu5/Desktop/23년 봄학기/퀀트입문(한희준)/R codes/R_basics")
dir()


## ----fread_select, 선택해서 불러들이기-------------------------------------

survey <- read.csv("old_survey.csv")
attach(survey)   # R에 장착(?)
survey

survey = cbind(handedness, height, handspan)

## ----cor: correlation, use = "complete.obs"은 모든 missing observations 제거 -----------
cor(handspan, height, use = "complete.obs")

## ----cor_dt--------------------------------------------------------------
cor(survey, use = "complete.obs")
#alternatively, there's the na.omit function. na.omit은 NA를 제거, 동일한 결과 
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

## ----function_write--------------------------------------------------- # 일종의 표준화 작업standardizing
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

## ----mean: R에 내장된 함수----------------------------------
mean(height, na.rm = TRUE)

## ----mymean: 위에서 정의된 함수-----------------------------------------
mymean(height)

## ----mymean2-------------------------------------------------------------
mymean2 = function(x){
  x.bar = sum(x, na.rm = TRUE)/length(x)
  return(x.bar)
}
mymean2(height)
# mymean과 mymean2의 비교
# 값이 다른 이유는 length(x)값이 mymean은 결측치 제거된 값이 들어가서 다르기 때문
# mymean2 함수 안에서 sum ignores missing observations but length does not.
 

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
# skewness 개념 반드시 검색해서 공부 필수(개념 및 나타나는 방식식)

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
  
  keep = !is.na(x) & !is.na(y) # 둘다 결측치가 없는
  x = x[keep]
  y = y[keep]
  
  n = length(x)
  
  s.xy = sum( (x - mean(x)) * (y - mean(y)) ) / (n-1) # n-1인 이유는 평균 낼 때 자유도 11을 뺏기기 때문
  return(s.xy)
}

## ----mycov 함수와 R에 내장된 함수 비교-----------------------------------------
cov(handspan, handedness, use = "complete.obs")
mycov(handspan, handedness)
