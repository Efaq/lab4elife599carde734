## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lab4elife599carde734)
library(caret)
library(mlbench)
library(leaps)



## -----------------------------------------------------------------------------
data("BostonHousing")

set.seed(123)
trainIndex <- createDataPartition(BostonHousing$medv, p = .8, 
                                  list = FALSE, 
                                  times = 1)

BostonTrain <- BostonHousing[ trainIndex,]
BostonTest  <- BostonHousing[-trainIndex,]

head(BostonTrain)
head(BostonTest)

## -----------------------------------------------------------------------------
lm_model <- train(medv ~ .,
               data = BostonTrain,
               method = "lm")


## -----------------------------------------------------------------------------
forward_selection  <- train(medv ~ ., data  =  BostonTrain ,
                             method  =  "leapForward" )


## -----------------------------------------------------------------------------

trainX  <-  BostonTrain[,-(which(names(BostonTrain)=="medv"))]
trainY  <-  BostonTrain$medv


testX  <-  BostonTest[,-(which(names(BostonTest)=="medv"))]
testY  <-  BostonTest$medv

predictions_lm <- predict(lm_model, BostonTest)
predictions_forward <- predict(forward_selection, BostonTest)


#RMSE - The lower the better

lm_model_rmse<- RMSE(predictions_lm,testY)
lm_forward_rmse<- RMSE(predictions_forward,testY)

#R2 (R-squared) - The higher the better

lm_model_r2<- R2(predictions_lm,testY)
lm_forward_r2<- R2(predictions_forward,testY)

#RMSE for Linear Regression
print(lm_model_rmse)

#RMSE for Linear Regression forward selection
print(lm_forward_rmse)

#R2 for Linear Regression
print(lm_model_r2)

#R2 for Linear Regression forward selection
print(lm_forward_r2)

#For both indicator, linear regression model seems to be the model that predicts better the data


