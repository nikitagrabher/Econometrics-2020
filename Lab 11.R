# Lab 11

## Set up
require(data.table)
require(sandwich)
require(lmtest)
require(stargazer)

## Let's do some more simulations. 

#### Start with a baseline case from the OVB setting  x1, x2 exogenous.
set.seed(4277)
x1 <- rnorm(n = 10000, mean = 0 , sd = 3) # create indep. var. 1
x2 <- rnorm(n = 10000, mean = 0, sd = 4) # create indep. var. 2
e <- rnorm(n = 10000, mean = 0, sd = 2) # create error
y <- 2 + 3*x1 + 4*x2 + e # create y according to population model

dt.population <- data.table( y, x1, x2) # creates tables
dt.population # shows first and last entries of table


summary(lm( y ~ x1 + x2, data = dt.population))
out.y.exog <- lm ( y ~ x1 + x2, data = dt.population) # exog model

### Endogeneous x2
set.seed(1984)
x1 <- rnorm(n = 1000, mean = 0 , sd = 3) # create indep. var. 1
x2a <- rnorm(n = 1000, mean = 0 , sd = 3) # create indep. var. 2 - exogeneous part
x2e <- rnorm(n = 1000, mean = 0 , sd = 2) # create indep. var. 2 - endogeneous
x2 <- x2a/2+x2e/2
e <- rnorm(n = 1000, mean = -0.5*x2e  , sd = 1.5) # create error
y <- 2 + 3*x1 + 4*x2 + e # create y according to population model

plot(e,x2)
cor.test(x = e, y = x2)

dt.pop_endog <- data.table( y, x1, x2) # creates tables
dt.pop_endog # shows first and last entries of table

out.y.endog <- lm ( y ~ x1 + x2, data = dt.pop_endog) # endog model
stargazer(out.y.exog, out.y.endog, type="text")

### Let's "find" instruments
z1a <- rnorm(n = 1000, mean = 0.01*x2a , sd = 2.5) # create weak instrument z1 (Assumption iv.2 essentially violated - irrelevant)
z1b <- rnorm(n = 1000, mean = 0.8*x2a , sd = 0.4) # create instrument z1
z1c <- rnorm(n = 1000, mean = 0.6*x2a + 0.5*e, sd = 1.5) # create invalid instrument z1 (assumption iv.1 violated (exogeneity)) 

### Look at instruments: which one should we pick?
plot(z1a,x2)
cor.test(x = z1a, y = x2)

plot(z1b,x2)
cor.test(x = z1b, y = x2)

plot(z1c,x2)
cor.test(x = z1c, y = x2)

### Pick an instrument
z1 <- z1b
dt.pop_iv <- data.table( y, x1, x2, z1a, z1b, z1c, z1) # creates tables
dt.pop_iv # shows first and last entries of table

### IV on foot (as in the slides)
cov(z1, x2)
cov(z1, y)

Den = cov(z1, x2)
Num = cov(z1, y)

iv_foot = Num/Den 
iv_foot

# Note that this is not fully accurate, because I would have to use the multivariate estimator.

### Now the 2SLS iv:
out1st <- lm(x2 ~  z1, data= dt.pop_iv )
summary(out1st)
dt.pop_iv <- dt.pop_iv[, x2hat:=predict(out1st, newdata=dt.pop_iv)]
dt.pop_iv

out2nd <- lm(y ~x1 + x2hat, data= dt.pop_iv )
summary(out2nd)

stargazer(out1st, out2nd, type="text")
stargazer(out.y.exog, out.y.endog, out2nd, type="text")

### R has this inbuilt as well
library(ivreg)
ivB <- ivreg(y~x1+x2|x1+ z1, data=dt.pop_iv)
summary(ivB)
stargazer(out1st, out2nd, ivB, type="text")

# If you compare the 2sls and the R-Routine it is exactly identical. 

### Let's take a look at the other 2 candidates: 

# Weak IV is much worse
iv_weak <- ivreg(y~x1+x2| x1+ z1c, data=dt.pop_iv)
stargazer(out1st, out2nd, iv_weak, type="text")

# Endogenous IV is even worse
iv_wrong <- ivreg(y~x1+x2| x1+ z1a, data=dt.pop_iv)
stargazer(out1st, out2nd, iv_wrong, type="text")

### Lets compare
stargazer(out.y.exog, out.y.endog, ivB, iv_weak, iv_wrong, type="text")

# IV does a good job (even though not perfect) at correcting the bias, when you get it exactly right. 
# However, weak IV andendogenous IV can induce a bias that is worse than no correction.  

### Outlook: Example for an over-identified IV (would require more time to properly cover) 
iv_over <- ivreg(y~x1+x2| x1+ z1 + z1a + z1c, data=dt.pop_iv)
stargazer(out1st, out2nd, iv_over, type="text")


