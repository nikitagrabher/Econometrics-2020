# Getting started

## Using the console as a calculator

1+1
3*4
24/6
(2*10)-(3*4)
2^3
8^(1/3)
sqrt(25)
log(1)

my.sum <- 10+10
my.sum
my.sum/10


## Loading data into RStudio

setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 1")
load("ceosal2.RData")


## Loading data into RStudio

install.packages("data.table")
library(data.table)

dt.ceo.salaries <- data.table(data)
rm(data)

names(dt.ceo.salaries)
colnames(dt.ceo.salaries)
ncol(dt.ceo.salaries)
nrow(dt.ceo.salaries)

head(dt.ceo.salaries)
tail(dt.ceo.salaries)

view(dt.ceo.salaries)
dt.ceo.salaries[1, ]
dt.ceo.salaries[ , salary]
dt.ceo.salaries[1, salary]
dt.ceo.salaries[1:10, list(salary, age)]
dt.ceo.salaries[order(age)]
dt.ceo.salaries[order(-age)]
dt.ceo.salaries[age<=45,]

dt.young.ceo.salaries <- dt.ceo.salaries[age<=45,]
dt.ceo.salaries[age<=45 & grad==1,]

dt.ceo.salaries[, log_salary:=log(salary)]
dt.ceo.salaries[, age_squared:=age^2]
dt.ceo.salaries[, log_salary:=NULL]







