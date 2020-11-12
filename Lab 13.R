# Lab 13

# Install Packages

install.packages("tidyverse")
install.packages("gplots")
install.packages("tseries")

# Load packages

library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programming tools for plotting data
library(tseries)   # For time series analysis
library(lmtest)    # For heteroskedasticity analysis

# Data Import and Tidying
dataPanel101 <- read.csv("https://github.com/ds777/sample-datasets/blob/master/dataPanel101.csv?raw=true")

dataPanel101 <- pdata.frame(dataPanel101, index=c("country","year"))

# View tabular data
dataPanel101

# Exploratory Data Analysis
coplot(y ~ year|country, type="b", data=dataPanel101) 
scatterplot(y~year|country, data=dataPanel101)
plotmeans(y ~ country, data = dataPanel101)

#Heterogeneity across years
plotmeans(y ~ year, data = dataPanel101)

#4 Panel Data Modeling
#4.1 Basic OLS model
#The basic OLS regression model does not consider heterogeneity across countries or across years
ols <-lm(y ~ x1, data = dataPanel101)
summary(ols)

# Fitted values
yhat <- ols$fitted
ggplot(dataPanel101, aes(x = x1, y = y))+
  geom_point() +
  geom_smooth(method=lm)

#4.2 Fixed Effects Model

#4.2.1 Country-Specific Fixed Effects using Dummy Variables (LSDV Model)
fixed.dum <-lm(y ~ x1 + factor(country) - 1, data = dataPanel101)
summary(fixed.dum)

#Fit
yhat <- fixed.dum$fitted
scatterplot(yhat ~ dataPanel101$x1 | dataPanel101$country,  xlab ="x1", ylab ="yhat", boxplots = FALSE,smooth = FALSE)
abline(lm(dataPanel101$y~dataPanel101$x1),lwd=3, col="red")

#4.2.1.1 OLS vs LSDV
#Each component of the factor variable (country) is absorbing the effects particular to each country. Predictor x1 was not significant in the OLS model, once controlling for differences across countries, x1 became significant in the OLS_DUM (i.e. LSDV model)

# Country-Specific Fixed Effects using the plm package
fixed <- plm(y ~ x1, data=dataPanel101, model="within")
summary(fixed)

# Display the fixed effects (constants for each country)
fixef(fixed)

###The coeff of x1 indicates how much Y changes overtime, on average per country, when X increases by one unit.

#4.2.2.1 Fixed Effects vs OLS
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed, ols)

###If the p-value is < 0.05 then the fixed effects model is a better choice

#4.3 Random Effects Model
random <- plm(y ~ x1, data=dataPanel101, model="random")
summary(random)

##Interpretation of the coefficients is tricky since they include both the within-entity and between-entity effects. In the case of TSCS data represents the average effect of X over Y when X changes across time and between countries by one unit.
## Also remember that the Random Effects assumptions are much stronger. 

#4.4 Fixed vs Random
#To decide between fixed or random effects you can run a Hausman test where the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects (see Green, 2008, chapter 9). It basically tests whether the unique errors are correlated with the regressors, the null hypothesis is they are not. If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.

phtest(fixed, random)

#=> We should use the random effects model

# 4.5 Regression Diagnostics
##4.5.1 Time-fixed effects testing
fixed.time <- plm(y ~ x1 + factor(year), data=dataPanel101, model="within")
summary(fixed.time)

# Testing time-fixed effects. The null is that no time-fixed effects are needed
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#=>If the p value < 0.05 then use time-fixed effects. In this example, no need to use time-fixed effects.


##4.5.4 Serial correlation testing
#Serial correlation tests apply to macro panels with long time series. Not a problem in micro panels (with very few years).
#H0) The null is that there is not serial correlation.

pbgtest(fixed)

#Because p-value > 0.05, we conclude that there is NO serial correlation

##4.5.5 Unit roots/stationarity testing
#The Dickey-Fuller test to check for stochastic trends.

#H0) The null hypothesis is that the series has a unit root (i.e. non-stationary)
#If unit root is present you can take the first difference of the variable.

adf.test(dataPanel101$y, k=2)

#Because p-value < 0.05, we conclude that the series does NOT have unit root. In other words, the series is stationary


#4.5.6 Heteroskedasticity testing
#H0) The null hypothesis for the Breusch-Pagan test is homoskedasticity

bptest(y ~ x1 + factor(country), data = dataPanel101, studentize=F)

#Because p-value < 0.05, we detect hetersokedasticity
# => If hetersokedasticity is detected we need to use a robust covariance matrix (Sandwich estimator) to account for it

#4.5.6.1 Controlling for heteroskedasticity: Random effects
#The -vcovHC- function estimates three heteroskedasticity-consistent covariance estimators:
  
# "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
# "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
# arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.

# Original coefficients
coeftest(random) 
# Heteroskedasticity consistent coefficients
coeftest(random, vcovHC) 


## 4.5.6.2 Controlling for heteroskedasticity: Fixed effects

# Original coefficients
coeftest(fixed)
# Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, method = "arellano"))




# Acknowledgements and Thanks:
#This lab is based on 
#https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html#4562_controlling_for_heteroskedasticity:_fixed_effects
#and on material by M Godinho de Matos, R Belo and F Reis. Gratefully acknowledged! 
  