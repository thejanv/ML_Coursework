#I used unused object because of lack of memory of my system

#Removed all the existing objects
rm(list = ls())

library(readxl)

#Load the dataset
data <- read_excel("Data/uow_consumption.xlsx")
date <- data$date
data <- data[,4]
names(data)[1] <- "twenty"
acf(data)
pacf(data)
boxplot(data$twenty)
#Data pre-processing
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_nor <- as.data.frame(lapply(data, min_max_norm))

date_t <- factor(date)
date_t <- as.Date(date_t)
data_nor$date <- date_t

plot(data_nor$twenty~data_nor$date,type="l",col="blue")
library(xts)
library(zoo)
library(quantmod)
library(neuralnet)
library(forecast)
library(NeuralNetTools)

timeser <- xts(data_nor$twenty,order.by = as.Date(data_nor$date))
# rm(normalized_df,date,lable)
rtimeserall <- cbind(timeser,lag(timeser, 1),lag(timeser, 2),lag(timeser, 3),
                     lag(timeser, 4),lag(timeser, 7)
)
colnames(rtimeserall) <- c('t', 't1', 't2', 't3', 't4','t7')
rtimeserall <- rtimeserall[complete.cases(rtimeserall),]

data_train <- rtimeserall[1:370]
data_test <- rtimeserall[371:463]

# Remove missing values from the training data
#creating various kinds of neural networks and plotting them with the results matrix
set.seed(235)
consumsion_nnar_h1_all<- neuralnet(t ~ ., hidden = 1, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h1_all$result.matrix
plot(consumsion_nnar_h1_all, dimension = 8)


set.seed(236)
consumsion_nnar_h2_all<- neuralnet(t ~ ., hidden = 2, data = data_train,
                               linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2_all$result.matrix
plot(consumsion_nnar_h2_all)


set.seed(237)
consumsion_nnar_h3_all<- neuralnet(t ~ ., hidden = 3, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h3_all$result.matrix
plot(consumsion_nnar_h3_all)

set.seed(238)
consumsion_nnar_h4_all<- neuralnet(t ~ ., hidden = 4, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h4_all$result.matrix
plot(consumsion_nnar_h4_all)

set.seed(239)
consumsion_nnar_h5_t1.t7<- neuralnet(t ~ t1+t7, hidden = 5, data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5_t1.t7$result.matrix
plot(consumsion_nnar_h5_t1.t7)

set.seed(240)
consumsion_nnar_h1.1_t1.t7<- neuralnet(t ~ t1+t7, hidden = c(1,1), data = data_train,
                            linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h1.1_t1.t7$result.matrix
plot(consumsion_nnar_h1.1_t1.t7)



set.seed(241)
consumsion_nnar_h2.2_t1.t2<- neuralnet(t ~ t1+t2, hidden = c(2,2), data = data_train,
                               linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2.2_t1.t2$result.matrix
plot(consumsion_nnar_h2.2_t1.t2, dimension = 8)


set.seed(242)
consumsion_nnar_h3.3_t1.t2<- neuralnet(t ~ t1+t2, hidden = c(3,3), data = data_train,
                                 linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h3.3_t1.t2$result.matrix
plot(consumsion_nnar_h3.3_t1.t2)


set.seed(243)
consumsion_nnar_h4.4_t2.t3<- neuralnet(t ~ t2+t3, hidden = c(4,4), data = data_train,
                                  linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h4.4_t2.t3$result.matrix
plot(consumsion_nnar_h4.4_t2.t3)

set.seed(244)
consumsion_nnar_h5.5_all<- neuralnet(t ~ ., hidden = c(5,5), data = data_train,
                                  linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5.5_all$result.matrix
plot(consumsion_nnar_h5.5_all)

set.seed(245)
consumsion_nnar_h2.3_all<- neuralnet(t ~ ., hidden = c(2,3), data = data_train,
                                        linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h2.3_all$result.matrix
plot(consumsion_nnar_h2.3_all)

set.seed(246)
consumsion_nnar_h5.3_t3.t4<- neuralnet(t ~ t3+t4, hidden = c(5.3), data = data_train,
                                      linear.output=TRUE,threshold= 0.01)
consumsion_nnar_h5.3_t3.t4$result.matrix
plot(consumsion_nnar_h5.3_t3.t4)


model_results <- predict(consumsion_nnar, data_test)   
predictions <- model_results$net.result   
