# LAB 9

## Setup

require(data.table)
require(sandwich)
require(lmtest)
require(stargazer)

set.seed(1984)
x1 <- rnorm(n = 10000, mean = 0 , sd = 3) 
x2 <- rnorm(n = 10000, mean = 0, sd = 4)
e <- rnorm(n = 10000, mean = 0, sd = 2)
y <- 2 + 3*x1 + 4*x2 + e

dt.population <- data.table( y, x1, x2)
dt.population <- dt.population[order(y)]
dt.population

r.sample.rows <- sample(1:nrow(dt.population), size = 100)
r.sample.rows 

r.sample <- dt.population[r.sample.rows,]
head(r.sample)

summary(lm( y ~ x1 + x2, data = r.sample))

## Violation of Simple Linear Regression Assumption 3

### SLR.3: The sample outcomes x ,xi, i = 1,. . . ,n are not all the same value.

### Collinearity: The variable x2 is a constant.
set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 2) 
x2 <- rep(3, times=1000)
e <- rnorm(n = 1000, mean = 0, sd = 1)
y <- 2 + 3*x1 + 4*x2 + e

summary(lm( y ~ x1 + x2 ))

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 2)
x2 <- 0.4*x1 + rnorm(n=1000, mean = 0, sd =0.01)
e <- rnorm(n = 1000, mean = 0, sd = 1) 
y <- 2 + 3*x1 + 4*x2 + e

cor.test(x2,x1)

summary(lm( y ~ x1 + x2 ))

## Violation of Simple Linear Regression Assumption 4

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3)
x2 <- rnorm(n = 1000, mean = x1 , sd = 5)
plot(x1,x2)

cor.test(x = x1, y = x2)

e <- rnorm(n = 1000, mean = 0, sd = 1)
y <- 2 + 3*x1 + 4*x2 + e
out.y.full <- lm ( y ~ x1 + x2)
out.y.x1.om <- lm ( y ~ x1)
stargazer(out.y.full, out.y.x1.om, type="text")

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3) 
x2 <- rnorm(n = 1000, mean = -x1 , sd = 5)
plot(x1,x2)

cor.test(x = x1, y = x2)

e <- rnorm(n = 1000, mean = 0, sd = 1) # create error
y <- 2 + 3*x1 - 4*x2 + e
out.y.full <- lm ( y ~ x1 + x2) 
out.y.x1.om <- lm ( y ~ x1) 
stargazer(out.y.full, out.y.x1.om, type="text")

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3) 
x2 <- rnorm(n = 1000, mean = x1/3 , sd = 5) 
plot(x1,x2)

cor.test(x = x1, y = x2)

e <- rnorm(n = 1000, mean = 0, sd = 1)
y <- 2 + 3*x1 - 4*x2 + e
out.y.full <- lm ( y ~ x1 + x2)
out.y.x1.om <- lm ( y ~ x1)
stargazer(out.y.full, out.y.x1.om, type="text")

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3)
x2 <- rnorm(n = 1000, mean = -x1 , sd = 5)
plot(x1,x2)

cor.test(x = x1, y = x2)

e <- rnorm(n = 1000, mean = 0, sd = 1)
y <- 2 + 3*x1 + 4*x2 + e
out.y.full <- lm ( y ~ x1 + x2)
out.y.x1.om <- lm ( y ~ x1)
stargazer(out.y.full, out.y.x1.om, type="text")

out.y.full <- lm ( y ~ x1 + x2)
coeffs.full <- coefficients(out.y.full)
b2_hat <- coeffs.full[3]
b1_hat <- coeffs.full[2]
out.part.x2 <- lm ( x2 ~ x1)
coeffs.part <- coefficients(out.part.x2)
delta <- coeffs.part[2]
bias <- delta*b2_hat
bias

b1_tilda = b1_hat + bias
b1_tilda

## Violation of Simple Linear Regression Assumption 5

set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3)
x2 <- rnorm(n = 1000, mean = 0 , sd = 4)
e <- rnorm(n = 1000, mean = 0, sd = 1)
s <- exp(0.4 * x1 )

yhet <- 2 + 3*x1 + 4*x2 + s*e

plot(x1, yhet)

summary(out.lm.het <- lm( yhet ~ x1 + x2))

plot(y = residuals(out.lm.het), x=x1)

ynorm <- 2 + 3*x1 + 4*x2 + e
summary(out.lm <- lm( ynorm ~ x1 + x2))

plot(y = residuals(out.lm), x=x1)

bptest( ynorm ~ x1 + x2 )
bptest( yhet ~ x1 + x2 )

vcov(out.lm)
diag(vcov(out.lm))

sqrt( diag(vcov(out.lm)) )
sqrt( diag(vcovHC(out.lm)) )

vcovHC(out.lm.het)
diag(vcovHC(out.lm.het))

sqrt( diag(vcov(out.lm.het)) )
sqrt( diag(vcovHC(out.lm.het)) )

coeftest(out.lm.het)

coeftest(out.lm.het, vcov. = vcovHC(out.lm.het))

### Extras

dt.population[1,]
dt.population[1:5,]

c <- c(1,7,8)
dt.population[c,]

head(dt.population[,1])
head(dt.population[,1:2])

dt.population[1,1]
dt.population[1:2,1:2]



