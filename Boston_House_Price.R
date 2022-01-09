#Importing Libraries

library(MASS)
library(dplyr)
library(GGally)
library(glmnet)
library(randomForest)
library(car)

#Analyzing Boston Data

data(Boston)
summary(Boston)
boxplot(Boston)
boxplot(Boston$medv)

#Correlation
cor(Boston)
ggcorr(Boston, nbreaks=4,label = TRUE, label_size = 3)

#Check Conditions to see if Linear Regression can be used

#Independence of observations i.e. no autocorrelation
cor(Boston, Boston$medv)
#Normality
hist(Boston$medv, col = "Light Green", xlab="Median Value", 
     main="House Pricing")

## Different Regression Models

# Training and Testing

# Partitioning the data on a 8/2 ratio as training/test data sets
set.seed(123456)
sample_data <- sample(nrow(Boston),nrow(Boston)*0.80)
training_set <- Boston[sample_data,]
test_set <- Boston[-sample_data,]

# Variable Selection
#Can be done in 3 ways

#Forward Selection
nullmodel <- lm(medv~1, data = training_set)
fullmodel <- lm(medv~., data = training_set)
forward <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), 
                direction='forward')
summary(forward)

#Backward Elimination
backward <- step(fullmodel,direction='backward')

summary(backward)

#Step wise Selection
stepwise <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), 
                 direction='both')
summary(stepwise)

#Linear Regression

## Model 1
model_1 <- lm(medv~log(lstat)+rm,data = training_set)
pred_1 <- predict(model_1, newdata = test_set)
summary(model_1)
plot(model_1)
step(model_1)

## Model 2
model_2 <- lm(medv~rm,data = training_set)
pred_2 <- predict(model_2, newdata = test_set)
summary(model_2)
plot(model_2)
step(model_2)

## Model 3
model_3 <- lm(medv~lstat,data = training_set)
pred_3 <- predict(model_3, newdata = test_set)
summary(model_3)
plot(model_3)
step(model_3)

## Model 4
model_4 <- lm(medv~log(lstat)+rm+log(crim),data = training_set)
pred_4 <- predict(model_4, newdata = test_set)
summary(model_4)
plot(model_4)
step(model_4)

## Model 5
model_5 <- lm(medv~poly(lstat, 2),data = training_set)
pred_5 <- predict(model_5, newdata = test_set)
summary(model_5)
plot(model_5)
step(model_5)

## Model 6
model_6 <- lm( medv ~ .,data = training_set )
pred_6 <- predict(model_6, newdata = test_set)
summary(model_6)
plot(model_6)
step(model_6)

## Model 7
#Using the selected variables
model_7 <- lm( medv ~ crim + zn + chas + nox + rm + dis + ptratio + 
                 rad + black + lstat + tax ,data = training_set )
pred_7 <- predict(model_7, newdata = test_set)
summary(model_7)
plot(model_7)
step(model_7)

summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared 
summary(model_3)$adj.r.squared 
summary(model_4)$adj.r.squared 
summary(model_5)$adj.r.squared 
summary(model_6)$adj.r.squared
summary(model_7)$adj.r.squared
