# Load the "nnet" package
library(nnet)

# Generate some example data
rm(list = ls())

library(readxl)
library(xts)
library(zoo)
library(quantmod)
library(neuralnet)
library(forecast)
library(NeuralNetTools)

#Load the dataset
data <- read_excel("Data/uow_consumption.xlsx")
date <- data$date
data <- data[,2:4]

acf(data)
pacf(data)
#Data pre-processing
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_nor <- as.data.frame(lapply(data, min_max_norm))

date_t <- factor(date)
date_t <- as.Date(date_t)
data_nor$date <- date_t
names(data_nor)[1] <- "18h"
names(data_nor)[2] <- "19h"
names(data_nor)[3] <- "20h"

timeser_18 <- xts(data_nor$"18h",order.by = as.Date(data_nor$date))
timeser_19 <- xts(data_nor$"19",order.by = as.Date(data_nor$date))
timeser_20 <- xts(data_nor$"20",order.by = as.Date(data_nor$date))

#Creating Loads for 18h and 19h
timeser6_1 <- lag(timeser_18, 1)
timeser6_2 <- lag(timeser_18, 2)
timeser6_3 <- lag(timeser_18, 3)
timeser6_4 <- lag(timeser_18, 4)
timeser6_7 <- lag(timeser_18, 7)

timeser7_1 <- lag(timeser_19, 1)
timeser7_2 <- lag(timeser_19, 2)
timeser7_3 <- lag(timeser_19, 3)
timeser7_4 <- lag(timeser_19, 4)
timeser7_7 <- lag(timeser_19, 7)

timeser8_1 <- lag(timeser_20, 1)
timeser8_2 <- lag(timeser_20, 2)
timeser8_3 <- lag(timeser_20, 3)
timeser8_4<- lag(timeser_20, 4)
timeser8_7 <- lag(timeser_20, 7)


io <- cbind(timeser6_1,timeser6_2,timeser6_3,timeser6_4,timeser6_7,
                              timeser7_1,timeser7_2,timeser7_3,timeser7_4,timeser7_7,
                              timeser8_1,timeser8_2,timeser8_3,timeser8_4,timeser8_7,timeser_20)
colnames(io) <- c('t6_1','t6_2','t6_3','t6_4','t6_7',
                              't7_1','t7_2','t7_3','t7_4','t7_7',
                              't8_1','t8_2','t8_3','t8_4','t8_7','target')
io <- io[complete.cases(io),]
data_train <- io[1:370]
data_test <- io[371:463]

relation_1 <- as.formula("target~ .")
relation_2 <- as.formula("target~ t6_1 + t7_1 + t8_1 + t6_7 + t7_7 + t8_7")
relation_3 <- as.formula("target~ t6_2 + t7_2 + t8_2 + t6_7 + t7_7 + t8_7")

# Define the NARX model architecture
set.seed(1121)
narx_net <- neuralnet(relation_1, data = data_train, hidden = c(5,3), 
                      act.fct = "logistic", linear.output = TRUE)
plot(narx_net)
#1
test(narx_net)

set.seed(1122)
narx_net <- neuralnet(relation_2, data = data_train, hidden = c(5,3), 
                      act.fct = "logistic", linear.output = FALSE)
#2
plot(narx_net)
test(narx_net)

#3
set.seed(1123)
narx_net <- neuralnet(relation_3, data = data_train, hidden = c(5,3), 
                      act.fct = "logistic", linear.output = FALSE)
plot(narx_net)
test(narx_net)
predict_expected_plot(narx_net)
title("Predicted vs Expected val NARX 3")


set.seed(1124)
narx_net <- neuralnet(relation_1, data = data_train, hidden = c(10,5), 
                      act.fct = "logistic", linear.output = FALSE)
#4
plot(narx_net)
test(narx_net)

narx_net <- neuralnet(relation_2, data = data_train, hidden = c(6,4), 
                      act.fct = "logistic", linear.output = FALSE)
#5
plot(narx_net)
test(narx_net)

narx_net <- neuralnet(relation_1, data = data_train, hidden = c(10,8), 
                      act.fct = "logistic", linear.output = FALSE)
#6
plot(narx_net)
test(narx_net)


predict_expected_plot <- function(narx_model) {
  # Use the model to make predictions on new data
  uow_load_modelres<-predict(narx_model,data_test)
  uow_load_modelres
  uow_loadtrain <- data[7:377,1]
  uow_loadtest <- data[378:470,1]
  names(uow_loadtest)[1] <- "20h"
  train_min<-min(uow_loadtest)
  train_max<-max(uow_loadtest)
  
  deNorm<-function(y,min,max){
    return((max-min)*y+min)
  }
  uow_load_pred_unnorm<- deNorm(uow_load_modelres,train_min,train_max)
  uow_load_pred_unnorm
  
  plot(uow_loadtest$"20h",ylab="Predicted vs Expected AR",type='l',col="red")
  par(new=TRUE)
  plot(uow_load_pred_unnorm,ylab='',yaxt='n',type='l',col='green',main='Predicted vs Expected val AR')
  legend("topleft",
         c("Expected","Predicted"),
         fill=c("red","green")
  )
}

#best narx index 4
#best AR 9