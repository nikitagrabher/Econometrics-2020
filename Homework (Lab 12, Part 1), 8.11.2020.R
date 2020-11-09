### LAB 12

## OLS Regression on foot

setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 12")

library(data.table)


df.OLSdat <- read.table("OLSqData.RData")
DT <- data.table(df.OLSdat) 

# Take a look at the data, which variables do you have?

summary(df.OLSdat)
summary(DT)

# Now use cbind to build 2 matrices: o	Using DT$const, DT$x1, DT$x2 and DT$x3 to build matrix X and o	Using DT$y5 to build matrix y 
X <- cbind(c(DT$const), c(DT$x1), c(DT$x2), c(DT$x3))
X

y <- cbind(c(DT$y1))
y

# Now compute X’ (transpose of X) 
Xt <- t(X)
Xt

# Next compute X’X . Report your result.
XtX <- Xt%*%X
XtX

# Next compute the inverse of X’X. Report it.
invXtX <- (XtX)^-1
invXtX

# Next compute X’y. Report it.
Xty <- Xt%*%y
Xty

# Lastly multiply the inverse of X’X with X’y. Report your result. What is this?
OLS <- invXtX%*%Xty
OLS 

# Finally run a few comparisons: Regress y1 on x1, x2, and x3 in a linear model. 
OLS1 <- lm(y1 ~ x1 + x2 + x3, data=DT)
summary(OLS1)

# Divide XtX by n (number of observations) and compare it to cov(DT), comment. 
XtX/7584

covDT <- cov(DT)
covDT






