##### R basic 2 #####

#####################
## 데이터 형태 
#####################

# Numeric (수치형), Logical (논리형), Character (문자형), Comlex (복소수형)

# mode() 또는 class() 데이터 유형 출력
mode(3)
mode(3>4)
mode(TRUE)
mode("퀀트")
mode(3+2i)

# 데이터 유형 검증
# is.numeric(x), is.double(x), is.interger(x), is.logical(x), is.character(x), is.complex(x), is.na(x), is.null(x), is.nan(x), is.finite(x), is.infinite(x), is.matarix(x)
# NA (결측치, Not Available or Missing Value)
# NULL 비어있는 값
# NaN 수학적으로 정의가 불가능한 수 Not a Number
# Inf 무한대 

is.double(2.2)  # 실수형 여부  
is.integer(2.2) # 정수형 여부
is.integer(1) # 예외
is.integer(1L) # 이렇게 표시해야 정수로 인식
is.integer(as.integer(1))

# 데이터 유형 변경
# as.numeric(x), as.double(x), as.integer(x), as.logical(x), as.character(x), as.complex(x), as.matrix(x)

is.numeric(1/3)
as.character(1/3)
a = "A"
as.numeric("A") #error

b = as.character(2)
b
b-5 #error

c = "11"
c-5
as.numeric(c)-5



#############################

setwd("C:/Users/boyu5/Documents/R codes/R_basics")
dir()

survey <- read.csv("old_survey.csv")
attach(survey)   # R에 장착(?)

## ----add_column, ----------------------------------------
height_handspan_ratio = height/handspan
survey = cbind(survey, height_handspan_ratio)

## ----remove_column, -------------------------------------
survey = survey[,-c(7:10)]

## ----head_tail-----------------------------------------------------------
head(survey)
tail(survey)

## ----hist_plain, 히스토그램 ----------------------------------------
hist(handedness)

## ----hist_dressed, --------------------------------------
hist(handedness, xlab = 'Handedness Score',
     main = 'Histogram of Handedness Scores',
     ylab = '# of Students')

## ----hist_breaks, 막대 20개로 조정  ---------------------------------------
hist(handedness, breaks = 20, xlab = 'Handedness Score',
     main = 'Histogram of Handedness Scores')

## ----hist_freq, y축 Density -----------------------------------------
hist(handedness, breaks = 20, freq = FALSE,
     xlab = 'Handedness Score', main = 'Histogram of Handedness')

## ----plot_basic, scatter plot ----------------------------------------
plot(height, handspan)
#plot(height, handspan, 'l') # line

## ----plot_rev, ------------------------------------------
plot(handspan, height)

## ----plot_ornament, -------------------------------------
plot(height, handspan, xlab = "height (in)", ylab = "handspan (cm)")

## ----plot_col, 색깔 ------------------------------------------
plot(height, handspan, xlab = "height (in)",
     ylab = "handspan (cm)", col = "red")

## ----plot_pch, 형태 ------------------------------------------
plot(height, handspan, xlab = "height (in)",
     ylab = "handspan (cm)", col = "red", pch = 3)

## ----plot_type_l, connected line ---------------------------------------
plot(height, handspan, xlab = "height (in)",
     ylab = "handspan (cm)", col = "red", pch = 3, type = 'l')

## ----pairs---------------------------------------------------------------
A = cbind(handedness, handspan, height)
pairs(A)

## ----boxplot, median, interquartile (25% and 75% quantile), min and max, outlier------------------
boxplot(handspan, ylab = "Handspan(cm)")

## ----boxplot_comparison--------------------------------------------------
boxplot(handspan ~ sex, ylab= "Handspan (cm)", main = "Handspan by Sex")



## ----summary-------------------------------------------------------------
summary(survey)

## ----sum_ missing observation이 있을 때------------------------------------
sum(survey$height)

## ----na.rm: ignore missing observations; NA를 제거하고 sum ----------------
sum(survey$height, na.rm = TRUE)

## ----mean_na.rm_1, mean 평균-----------------------------
mean(survey$height, na.rm = TRUE)

## ----mean_na.rm_2--------------------------------------------------------
#3 observations, 
mean(1:3)
#3 observations, but one missing observation --
#  missing observation을 무시하기 때문에, 평균은 (1+2)/2
mean(c(1, 2, NA), na.rm = TRUE)

## ----var_na.rm, variance 분산---------------------------------------------
var(survey$height, na.rm = TRUE)

## ----sd_na.rm, standard deviation 표준편차 -------------------------------
sd(survey$height, na.rm = TRUE)

## ----표준편차는 분산의 제곱근----------------------------------------------
sqrt(var(survey$height, na.rm = TRUE))

## ----median_na.rm, median 중위값 또는 중앙값-------------------------------
median(survey$height, na.rm = TRUE)
quantile(survey$height, na.rm = TRUE, probs = 0.5)

## ----quantile_5no, quantile 분위수 --------------------------------------
quantile(survey$height, na.rm = TRUE)

## ----quantile_probs, 분위수------------------------------------------------
quantile(survey$height, na.rm = TRUE, probs = 0.3)

## ----quantile_many_probs-------------------------------------------------
quantile(survey$height, na.rm = TRUE, probs = c(0.1, 0.3, 0.7, 0.9))

## ----iqr, Inter-Quartile Range---------------------------------------------
quantile(survey$height, na.rm = TRUE, probs = c(0, 0.25, 0.5, 0.75, 1.0))
IQR(survey$height, na.rm = TRUE) # 71-64

## ----iqr_w_quantile, ------------------------------------------------------
x = quantile(survey$height, na.rm = TRUE, probs = c(.25, .75))
x[2] - x[1]

## ----max_min, 최대 최소----------------------------------------------------
max(survey$height, na.rm = TRUE)
min(survey$height, na.rm = TRUE)

## ----range_by_hand-------------------------------------------------------
max(survey$height, na.rm = TRUE) - min(survey$height, na.rm = TRUE)

## ----range---------------------------------------------------------------
range(survey$height, na.rm = TRUE)

## ----which.max_min-------------------------------------------------------
which.max(survey$height)     # 몇번째 observation? 
survey$height[which.max(height)]
survey$height[which.min(height)]

## ----is_na  NA인지 여부 ---------------------------------------------------
x = c(1, 2, NA, 3, NA, 4)
is.na(x) 

## ----not-----------------------------------------------------------------
!is.na(x)

## ----and-----------------------------------------------------------------
y = c(NA, 1, NA, 2, 3, NA)
is.na(y)
!is.na(y)
!is.na(x) & !is.na(y)   #둘다 NA가 아님 
