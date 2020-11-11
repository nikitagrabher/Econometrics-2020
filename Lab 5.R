# Setup

setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 5")

require(GGally)
require(ggplot2)
require(data.table)
require(stargazer)

## Exercise 2

load("car.test.RData")
load("car.train.RData")

### 1
colnames(car.train)

car.train <- data.table(car.train)
car.train <- car.train[, Age2:=Age^2]
car.train <- car.train[, HP2:=HP^2] 
car.train <- car.train[, KM2:=KM^2] 
car.train <- car.train[, Weight2:=Weight^2]

out1 <- lm(Price ~ . , data = car.train)
summary(step(out1))

out <- lm( Price ~ Age + Age2 + KM + KM2 + HP + HP2 + Weight + Weight2 + FuelType + Automatic + CC
           , data = car.train)
stargazer(out, type = "text")

### 2
colnames(car.train)

### 3
car.test <- data.table(car.test)
car.test <- car.test[, Age2:=Age^2]
car.test <- car.test[, HP2:=HP^2]
car.test <- car.test[, Weight2:=Weight^2]

car.test <- car.test[, KM2:=KM^2]
car.test$y_hat <- predict(out1, newdata=car.test)
head(car.test)

### 4
library(Metrics)

car.train$y_hat <- predict(out1, newdata=car.train)
car.train$u_hat <- car.train$y_hat - car.train$Price
car.train$u_hat2 <- (car.train$u_hat)^2
head(car.train)

rmse.train <- sqrt(sum(car.train$u_hat2)/nrow(car.train))
rmse.train

rmse.train.2 <- rmse(car.train$Price, car.train$y_hat)
rmse.train.2

car.test$u_hat <- car.test$y_hat - car.test$Price
car.test$u_hat2 <- (car.test$u_hat)^2
head(car.test)

rmse.test <- sqrt(sum(car.test$u_hat2)/nrow(car.test))
rmse.test

rmse.test.2 <- rmse(car.test$Price, car.test$y_hat)
rmse.test.2

### Model 1
summary(out6a <- lm( log(Price) ~ Age, data=car.train))

coeffs.out6a <- coefficients(out6a)
logValue <- coeffs.out6a[1]
Value <- exp(logValue)
Value

delta <- 1 - exp(coeffs.out6a[2])
delta

### Model 2
summary(out6b <- lm( Price ~ Age, data=car.train))

coeffs.out6b <- coefficients(out6b)
Value <- coeffs.out6b[1]
alpha <- coeffs.out6b[2]/coeffs.out6b[1]

summary(out6a)$sigma

alpha <- exp((summary(out6a)$sigma)^2/2)
car.train$logyhat <- predict(out6a, newdata= car.train)
car.train$yhat <- alpha*exp(car.train$logyhat)
cor(car.train$yhat, car.train$Price)^2

summary(out6b)$r.squared




