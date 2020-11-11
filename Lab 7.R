# LAB 7

## Setup

setwd("/Users/nikitagrabher-meyer/Desktop/PHD/Econometrics/Labs/Lab 7")

require(GGally)
require(ggplot2)
require(data.table)
require(stargazer)

## Exercise 1

load("car.test.RData")
load("car.train.RData")

### 1. Use the dataset carTrain.RData to build a model explaining the price of used cars

### 1.a Explore the dataset and obtain descriptive statistics
dt.car.train <- data.table(car.train)
rm(car.train)

dt.car.test <- data.table(car.test)
rm(car.test)

qplot( data = dt.car.train
       , x = Price
       , geom = "histogram")

cor(dt.car.train[, list(Price, Age, KM, HP, CC, Doors, Weight)])

qplot( data = dt.car.train
       , x = Age
       , y = Price
       , geom = "point")

### 1.b How good is Age at predicting Price?
out0 <- lm( Price ~ Age , data = dt.car.train)
stargazer(out0, type = "text")

### 1.c Use the function step to improve your prediction model
out1 <- lm(Price ~ . , data = dt.car.train)
summary(step(out1))

### 2. Did you use all the variables in the dataset to build your model? Why?
out2 <- lm(Price ~ Age + KM + FuelType + HP + Automatic + CC + Weight , data = dt.car.train)

### 3. Use your model to predict used car prices in the datset carTest.RData.
dt.car.test <- dt.car.test[, yhat:=predict(out2, newdata=dt.car.test)]
head(dt.car.test)

### 4. Use the RMSE to compare the performance of your model in carTrain.RData and carTest.RData.
dt.car.train <- dt.car.train[, yhat:=predict(out2, newdata=dt.car.train)]
dt.car.train <- dt.car.train[, uhat:=yhat-Price]
dt.car.train <- dt.car.train[, uhat2:=uhat^2]
n.train <- nrow(dt.car.train)
rmse.train <- sqrt(sum(dt.car.train$uhat2)/n.train)
rmse.train

dt.car.test <- dt.car.test[, yhat:=predict(out2, newdata=dt.car.test)]
dt.car.test <- dt.car.test[, uhat:=yhat-Price]
dt.car.test <- dt.car.test[, uhat2:=uhat^2]
n.test <- nrow(dt.car.test)
rmse.test <- sqrt(sum(dt.car.test$uhat2)/n.test)
rmse.test






