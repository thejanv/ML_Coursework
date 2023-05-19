#Removed all the existing objects
rm(list = ls())

library(readxl)

#Load the dataset
data <- read_excel("Data/uow_consumption.xlsx")
date <- data$date
data <- data[,4]
names(data)[1] <- "20h"
acf(data)
pacf(data)
boxplot(data$"20h")
#Data pre-processing
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_nor <- as.data.frame(lapply(data, min_max_norm))
names(data_nor)[1] <- "20h"
boxplot(data_nor)
date_t <- factor(date)
date_t <- as.Date(date_t)
data_nor$date <- date_t

plot(data_nor$"20h"~data_nor$date,type="l",col="blue")
library(xts)
library(zoo)
library(quantmod)
library(neuralnet)
library(forecast)
library(NeuralNetTools)

timeser <- xts(data_nor$"20h",order.by = as.Date(data_nor$date))

# rm(normalized_df,date,lable)
rtimeserall <- cbind(timeser,lag(timeser, 1),lag(timeser, 2),lag(timeser, 3),
                     lag(timeser, 4),lag(timeser, 7))
colnames(rtimeserall) <- c('target', 't1', 't2', 't3', 't4','t7')
rtimeserall <- rtimeserall[complete.cases(rtimeserall),]

data_train <- rtimeserall[1:370]
data_test <- rtimeserall[371:463]


library(Metrics)
# Remove missing values from the training data
#creating various kinds of neural networks and plotting them with the results matrix
#1
set.seed(235)
consumsion_nnar_h1_all<- neuralnet(target ~ ., hidden = 1, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h1_all$result.matrix
plot(consumsion_nnar_h1_all, dimension = 8)
test(consumsion_nnar_h1_all)

#2
set.seed(236)
consumsion_nnar_h2_all<- neuralnet(target ~ ., hidden = 2, data = data_train,
                               linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2_all$result.matrix
plot(consumsion_nnar_h2_all)
test(consumsion_nnar_h2_all)

#3
set.seed(237)
consumsion_nnar_h3_all<- neuralnet(target ~ ., hidden = 3, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h3_all$result.matrix
plot(consumsion_nnar_h3_all)
test(consumsion_nnar_h3_all)

#4
set.seed(238)
consumsion_nnar_h4_all<- neuralnet(target ~ ., hidden = 4, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h4_all$result.matrix
plot(consumsion_nnar_h4_all)
test(consumsion_nnar_h4_all)

#5
set.seed(239)
consumsion_nnar_h5_t1.t7<- neuralnet(target ~ ., hidden = 5, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5_t1.t7$result.matrix
plot(consumsion_nnar_h5_t1.t7)
test(consumsion_nnar_h5_t1.t7)

#6
set.seed(240)
consumsion_nnar_h1.1_t1.t7<- neuralnet(target ~ t1+t7, hidden = c(1,1), data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h1.1_t1.t7$result.matrix
plot(consumsion_nnar_h1.1_t1.t7)
test(consumsion_nnar_h1.1_t1.t7)

#7
set.seed(241)
consumsion_nnar_h2.2_t1.t2<- neuralnet(target ~ t1+t2, hidden = c(2,2), data = data_train,
                               linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2.2_t1.t2$result.matrix
plot(consumsion_nnar_h2.2_t1.t2, dimension = 8)
test(consumsion_nnar_h2.2_t1.t2)

#8
set.seed(242)
consumsion_nnar_h3.3_t1.t2<- neuralnet(target ~ t1+t2, hidden = c(3,3), data = data_train,
                                 linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h3.3_t1.t2$result.matrix
plot(consumsion_nnar_h3.3_t1.t2)
test(consumsion_nnar_h3.3_t1.t2)

#9
set.seed(243)
consumsion_nnar_h4.4_t2.t3<- neuralnet(target ~ t2+t3, hidden = c(4,4), data = data_train,
                                  linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h4.4_t2.t3$result.matrix
plot(consumsion_nnar_h4.4_t2.t3)
test(consumsion_nnar_h4.4_t2.t3)
predict_expected_plot(consumsion_nnar_h4.4_t2.t3)

#10
set.seed(244)
consumsion_nnar_h5.5_all<- neuralnet(target ~ ., hidden = c(5,5), data = data_train,
                                  linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5.5_all$result.matrix
plot(consumsion_nnar_h5.5_all)
test(consumsion_nnar_h5.5_all)

#11
set.seed(245)
consumsion_nnar_h2.3_all<- neuralnet(target ~ ., hidden = c(2,3), data = data_train,
                                        linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2.3_all$result.matrix
plot(consumsion_nnar_h2.3_all)
test(consumsion_nnar_h2.3_all)

#12
set.seed(246)
consumsion_nnar_h5.3_t3.t4<- neuralnet(target ~ t3+t4+t7, hidden = c(3,2), data = data_train,
                                      linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5.3_t3.t4$result.matrix
plot(consumsion_nnar_h5.3_t3.t4)
test(consumsion_nnar_h5.3_t3.t4)

#13
set.seed(246)
consumsion_nnar_h5.3_t3.t4<- neuralnet(target ~ ., hidden = c(4,3,2), data = data_train,
                                       linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5.3_t3.t4$result.matrix
plot(consumsion_nnar_h5.3_t3.t4)
test(consumsion_nnar_h5.3_t3.t4)

