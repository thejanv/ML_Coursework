predict_expected_plot <- function(model_val) {
  # Use the model to make predictions on new data
  uow_load_modelres<-predict(model_val,data_test)
  uow_loadtrain <- data[7:377,1]
  uow_loadtest <- data[378:470,1]
  names(uow_loadtest)[1] <- "20h"
  
  train_min<-min(uow_loadtest)
  train_max<-max(uow_loadtest)
  
  #de normalization
  deNorm<-function(y,min,max){
    return((max-min)*y+min)
  }
  uow_load_pred_unnorm<- deNorm(uow_load_modelres,train_min,train_max)
  
  # plot prdicted vs expectd graph
  plot(uow_loadtest$"20h",ylab="Predicted vs Expected",type='l',col="red")
  par(new=TRUE)
  plot(uow_load_pred_unnorm,ylab='',yaxt='n',type='l',col='green')
  legend("topleft",
         c("Expected","Predicted"),
         fill=c("red","green")
  )
}

# ecaluation matrx and accuracy result generator
test <- function(consumsion_nnar) {
  table_result <- c()
  #Model performance evluation
  load_model<-predict(consumsion_nnar,data_test)
  
  # get test and train data set without normalization
  uow_loadtrain <- data[7:377,1]
  uow_loadtest <- data[378:470,1]
  names(uow_loadtest)[1] <- "20h"
  
  #finding min and max vaues of trained dataset
  train_min<-min(uow_loadtest)
  train_max<-max(uow_loadtest)
  
  #function for de-normalized data
  deNorm<-function(y,min,max){
    return((max-min)*y+min)
  }
  
  #calculate accuracy
  predict=load_model*abs(diff(range(data_test$target)))+min(data_test$target)
  actual=data_test$target*abs(diff(range(data_test$target)))+min(data_test$target)
  compare=data.frame(predict,actual)
  deviation=((actual-predict)/actual)
  is.na(deviation)<-sapply(deviation,is.infinite)
  dev_NAomit<-na.omit(deviation)
  
  compare= data.frame(predict,actual,deviation)
  accuracy=1-abs(mean(dev_NAomit))
  table_result$accuracy <- accuracy
  uow_load_pred_unnorm<- deNorm(load_model,train_min,train_max)
  
  #RMSE
  table_result$rmse <- rmse(exp(uow_load_pred_unnorm),uow_loadtest$"20h")
  #MAE
  table_result$mae <- mean(abs(uow_load_pred_unnorm - uow_loadtest$"20h"))
  # sMAPE
  table_result$smape <- (1/nrow(uow_loadtest)) * sum(200 * 
                                                       abs(mean(uow_load_pred_unnorm) - uow_loadtest) /
                                                       (abs(mean(uow_load_pred_unnorm)) + 
                                                          abs(uow_loadtest)))
  print(table_result)
  
}
