##### R basic 1 #####

# comment: #
# execute: ctrl+enter (or ctrl+r) for windows, command+enter for Mac
# clear console: ctrl+l

# help   : ?function
?sum
?cos
?"="

getwd()
setwd("C:/Users/boyu5/Documents/R_basics")
dir()


## ----strings-------------------------------------------------------------
'��Ʈ��������а� ���Ի� ������ �ȳ��ϼ���'
#  strings with EITHER double or single quotes.
#  There is only a very minimal difference
"��Ʈ��������а� ���Ի� ������ �ݰ����ϴ�"


#== means "equal to"
x = 5
x == 5

## ----arithmetic----------------------------------------------------------
#���ϱ�
1 + 1
#����
8 - 4
#������
13/2
#���ϱ�
4*pi
#10��
2^10

## ----logicals------------------------------------------------------------
3 < 4
3 > 4
3 == 4

#!= means "not equal to"
3 != 4
4 >= 5
4 <= 5
2 + 2 == 5
10 - 6 == 4

## ----variables-----------------------------------------------------------
x = 42
x / 2
#if we assign something else to x,
#  the old value is deleted  ����� 
x = "��Ʈ�Թ�!"
x
x = 5
x == 5
foo = 3
bar = 5
foo.bar = foo + bar
foo.bar
foo.bar2 = 2 * foo.bar
foo.bar2
foo_bar = foo - bar
foo_bar

## ----vectors_1-----------------------------------------------------------
x = c(4, 7, 9)
x
y = c('a', 'b', 'c')
y

## ----vectors_2-----------------------------------------------------------
x = c(1, TRUE, "three")
x
# c �ȿ� ���ڰ� �ֱ� ������ 1�� TRUE�� ���ڷ� �ν�

## ----vector_arithmetic---------------------------------------------------
x = c(1, 2, 3)
x + 4
x/3
-x
x^3
y = c(3, 2, 1)
x - y    
x * y    # element by element
x/y      # element by element
x > 2
x >= 2

1<x<3
#�� ��� error
x>1 & x<3  # 1<x<3


a = c(-2.456, 3.6789, 5.2344566 )
ceiling(a) # �ø�
floor(a) # ����
trunc(a) # ����
round(a)
round(a, digits = 2)  #digits�ڸ� ���� �ݿø�

## ----functions-----------------------------------------------------------
x = c(1, 2, 3)
#sum: ������ element���� ���ϱ� 
sum(x)
#prod: ������ element���� ���ϱ�
prod(x)
#sqrt: ������ element�� ������ ������ 
sqrt(x)
y = c(-1, 2, 4)
#abs: ���밪
abs(y)
#exp: exponential function �����Լ�. exp(x) is e^x
exp(y)
#log: _natural_ logarithm (base e) �α��Լ�, �ڿ��α�
log(x)
log(x, base=10)  # �α��Լ� base=10

#
max(y)
min(y)
range(y) # �ּ� �ִ�
mean(x)
median(x)


## ----sequences-----------------------------------------------------------
x = 1:10 # 1=> 10
x
y = 10:1 # 10 => 1
y
# 0���� 1���� sequence, gap 0.02
z = seq(0, 1, by = .02)
z
# 0���� 1���� sequence, 10�� �յ� #ó���̶� ������ ����
w1 = seq(0, 1, length.out = 10)
w1
# 0���� 1���� sequence, 11�� �յ�
w2 = seq(0, 1, length.out = 11)
w2

## ----programming_functions-----------------------------------------------
x = 99:32
x
#length: x�� elements (items) ����
length(x)
y = c("��", "����", "����", "�ܿ�")
#������ ����?
mode(y)
#rep: �ݺ�
rep(y, 2)
rep(y,times=2)

rep(y,each=2)

#head/tail: ������ Ȯ�ο� ����
x = 1:100000
head(x)
tail(x)

## ----vector_extraction_1-------------------------------------------------
x = c(5, 4, 1)
x[1]
x[3]
x[1:2] # index 1, 2
x[2:3]

## ----vector_extraction_2-------------------------------------------------
x = 20:30
x
x[c(1, 3, 5, 7, 9)]
x[-c(1, 3, 5, 7, 9)] # ����
x[c(5, 9)]
x[seq(1, 10, by = 2)]


## ----vector_extraction_3-------------------------------------------------
x = c(5, 6, 7)
x[c(TRUE, TRUE, FALSE)]
x[c(FALSE, TRUE, FALSE)]
x[c(FALSE, FALSE, TRUE)]

## ----vector_extraction_4-------------------------------------------------
x = c(-1, 0, 1)
x > 0
x[x > 0]
x[x <= 0]

## ----vector_replacement--------------------------------------------------
x = c(-1, 5, 10)
x[3] = 4
x
x[x < 0] = 0
x


## ----packages--------------------------------------------
install.packages("data.table")

library(data.table)

## ----data.frame_1--------------------------------------------------------
foo = 1:5
bar = 2 * foo
foo.bar = data.frame(foo, bar)
foo.bar

## ----data.frame_2--------------------------------------------------------
y = -4:0
data.frame(foo, bar, y)

## ----subsetting_1--------------------------------------------------------
location = c("New York", "Chicago", "Boston", "Boston", "New York")
salary = c(70000, 80000, 60000, 50000, 45000)
title = c("Office Manager", "Research Assistant", "Analyst", "Office Manager", "Analyst")
hours = c(50, 56, 65, 40, 50)
jobsearch = data.frame(location, salary, title, hours)
jobsearch

## ----subsetting_2--------------------------------------------------------
subset(jobsearch, location == 'New York')

## ----subsetting_3--------------------------------------------------------
subset(jobsearch, salary > 50000)

## ----subsetting_4--------------------------------------------------------
subset(jobsearch, hours <= 50)

## ----data.table_extra_1--------------------------------------------------
person = c("Linus", "Snoopy", "Lucy", "Woodstock")
age = c(5, 8, 6, 2)
weight = c(40, 25, 50, 1)
my.data.table = data.frame(person, age, weight)
my.data.table

## ----data.table_extra_2--------------------------------------------------
age[1:2]
age[c(1,3)]

## ----data.table_extra_3--------------------------------------------------
my.data.table[1:2]
my.data.table[c(1, 3)]

## ----data.table_extra_4--------------------------------------------------
#what is the first row of the third column?
my.data.table[1, 3]
#what are the first three rows of the third column?
my.data.table[1:3, 3]

## ----data.table_extra_5--------------------------------------------------
my.data.table[ , 2:3]

## ----data.table_extra_6--------------------------------------------------
my.data.table[c(1,3), ]
my.data.table[c(1,3)]
my.data.table[,c(1,3)]

## ----data.table_extra_7--------------------------------------------------
my.data.table[["weight"]]

## ----data.table_extra_8--------------------------------------------------
my.data.table$weight

## ----data.table_extra_9--------------------------------------------------
my.data.table[ , c("person", "weight")]

## ----data.table_extra_10-------------------------------------------------
my.data.table[ , 2]
my.data.table[ , c(1,2)]
my.data.table[ , 1:2]


# write data
write.csv(my.data.table,"data1.csv")
write.table(my.data.table,"data1.txt")