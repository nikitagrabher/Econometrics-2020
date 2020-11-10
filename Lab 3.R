# Exercise 1

## Setup

require(data.table)
require(ggplot2)
require(stargazer)
require(Hmisc)

setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 3")
sales <- read.csv("sales-data.csv")
dt.sales <- data.table(sales)
rm(sales)

## Explore the data

ncol(dt.sales)
nrow(dt.sales)
colnames(dt.sales)
head(dt.sales)

stargazer(dt.sales, type = "text")
summary(dt.sales)

qplot( data = dt.sales
       , x = advertising
       , y = sales
       , geom = "point") +
  theme_bw()

dt.sales[, cor(sales, advertising)]

dt.sales[, rcorr(sales, advertising)]

## Simple regression analysis

lm.sales <- lm(sales ~ advertising, data=dt.sales)

summary(lm.sales)
stargazer(lm.sales, type = "text")

coeffs = coefficients(lm.sales)
coeffs

qplot( data = dt.sales
       , x = advertising
       , y = sales
       , geom = c("point", "smooth")
       , method = lm) +
  theme_bw() +
  labs( x = "advertising dollars", y = "sales dollars")

advertising = 100
sales = coeffs[1] + coeffs[2]*advertising
sales

my.budget = data.table(advertising=100)

predict(lm.sales, my.budget)

predict(lm.sales, my.budget, interval="predict")


# Exercise 2

## Hypothesis testing - Examples

### Example 1

### Population mean with known variance
alpha = .05 
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)

xbar = 1000/25 
mu0 = 45 
sigma = sqrt(500) 
n = 25 
z = (xbar-mu0)/(sigma/sqrt(n))
z

pnorm(z, lower.tail=FALSE)
pnorm(z, lower.tail=TRUE)
pval = 2 * pnorm(z)
pval

### Population mean with unknown variance
t.alpha = .05
t.half.alpha = qt(1-alpha/2, 25-1)
c(-t.half.alpha, t.half.alpha)

xbar = 1000/25 
mu0 = 45
s = sqrt(400) 
n = 25
t = (xbar-mu0)/(s/sqrt(n))
t

pt(t, df=25-1, lower.tail=FALSE)
pt(t, df=25-1, lower.tail=TRUE)
pval = 2 * pt(t, df=25-1)
pval

### Population variance
q.alpha = .05
q.half.alpha.up = qchisq(1-alpha/2, 25-1)
q.half.alpha.up

q.half.alpha.low = qchisq(alpha/2, 25-1) # critical values
q.half.alpha.low

sigma_sqr_0 = 500
s_sqr = 400
n = 25

Q = ((n-1)*s_sqr)/sigma_sqr_0
Q

### Example 2

### Proportion
alpha = .05
z.alpha = qnorm(1-alpha)
z.alpha

p0 = 0.60
fn = 47/61 
n = 61
z = (fn-p0)/sqrt((0.6*(1-0.6))/n)
z

### Example 3

### Difference in Population Means - Independent Samples and Variance unknown
nX = 21
nY = 25
sX = 1.30
sY = 1.16

Sp_sqr = ((nX - 1)*(sX^2) + (nY-1)*(sY^2))/(nX + nY - 2)
Sp_sqr

df = nX + nY - 2
df

alpha = .05
t.half.alpha = qt(1-alpha/2, 44)
c(-t.half.alpha, t.half.alpha)

xbar = 3.27
ybar = 2.53
t = ((xbar-ybar)-0)/sqrt((Sp_sqr/nX)+(Sp_sqr/nY))
t

pt(t, df=44, lower.tail=FALSE)
pt(t, df=44, lower.tail=TRUE)
pval = 2 * pt(t, df=44, lower.tail=FALSE)
pval

### Example 4

library(data.table)
setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 3")

dt.stocks <- data.table(read.csv("data_r.csv"))
dt.stocks <- setnames(dt.stocks, tolower(names(dt.stocks)))
head(dt.stocks)

xbar <- dt.stocks[, mean(idjcomp, na.rm=TRUE)]
s <- dt.stocks[, sd(idjcomp, na.rm=TRUE)]
n <- dt.stocks[, length(which(!is.na(idjcomp)))]
error <- qnorm(0.975)*s/sqrt(n)
left <- xbar-error
right <- xbar+error
left
right

dt.stocks[, t.test(isp500)]
dt.stocks[, t.test(isp500, alternative = c("greater"), mu=0.5, conf.level = 0.99)]
dt.stocks[, t.test(idjcomp, inasdaq, paired=TRUE)]
dt.stocks[, postcrisis:=ifelse(year>2008,1,0)]
dt.stocks[postcrisis==0, mean(idjcomp, na.rm=TRUE)]
dt.stocks[postcrisis==1, mean(idjcomp, na.rm=TRUE)]
dt.stocks[, t.test(idjcomp ~ postcrisis)]
dt.stocks[, t.test(idjcomp, inasdaq, var.equal=TRUE)]
dt.stocks[, t.test(idjcomp, inasdaq, var.equal=FALSE)]















