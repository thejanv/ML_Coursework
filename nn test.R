########## AR Approach ##########

#Model performance evluation
uow_load_modelres<-predict(consumsion_nnar,data_test)
uow_load_modelres
# get test and train data set without normalization
uow_loadtrain <- data[7:377,1]
uow_loadtest <- data[378:470,1]
#finding min and max vaues of trained dataset
train_min<-min(uow_loadtest)
train_max<-max(uow_loadtest)
#function for unormalized data
unNorm<-function(y,min,max){
  return((max-min)*y+min)
}
uow_load_pred_unnorm<- unNorm(uow_load_modelres,train_min,train_max)
uow_load_pred_unnorm
#testing pperformance of RMSE
library(Metrics)
rmse(exp(uow_load_pred_unnorm),uow_loadtest$twenty)
#testing pperformance of MSE
mse(exp(uow_load_pred_unnorm),uow_loadtest$twenty)
#testing pperformance of MAPE
mape(exp(uow_load_pred_unnorm),uow_loadtest$twenty)
#correlation between predicted and actual values
cor(uow_load_pred_unnorm,uow_loadtest$twenty)
#generate eleventh hour plot
par(mfrow=c(1,1))

plot(uow_loadtest$twenty,uow_load_pred_unnorm,col='green',main='unnormalized Prediction 
Graph AR',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend("bottomright",legend='NN',pch=18,col='green')
uow_load_finalres<-cbind(uow_loadtest,uow_load_pred_unnorm)
uow_load_finalres
plot(uow_loadtest$twenty,ylab="Predicted vs Expected AR",type='l',col="red")
par(new=TRUE)
plot(uow_load_pred_unnorm,ylab='',yaxt='n',type='l',col='green',main='Predicted vs Expected val AR')
legend("topleft",
       c("Expected","Predicted"),
       fill=c("red","green")
)
#calculate accuracy
predict=uow_load_modelres*abs(diff(range(data_test$t)))+min(data_test$t)
actual=data_test$t*abs(diff(range(data_test$t)))+min(data_test$t)
compare=data.frame(predict,actual)
deviation=((actual-predict)/actual)
deviation
is.na(deviation)<-sapply(deviation,is.infinite)
deviation
dev_NAomit<-na.omit(deviation)
dev_NAomit

compare= data.frame(predict,actual,deviation)
accuracy=1-abs(mean(dev_NAomit))
accuracy

#####NNARX APPROCH#########

#Generate NN in NARX
uow_load_nnarx<- neuralnet(eleven_h~date+eleven_h, 
                           hidden=c(3,2),data=uow_load_trainnorm,linear.output=TRUE,threshold= 0.01)
plot(uow_load_nnarx)

#model performance evaluation
uow_load_nraxmodel_res <-predict(uow_load_nnarx,uow_load_testnorm)
uow_load_nraxmodel_res
uow_load_pred_Narxunnorm<- unNorm(uow_load_nraxmodel_res,train_min,train_max)
uow_load_pred_Narxunnorm
#testing pperformance of RMSE
RMSE(exp(uow_load_pred_Narxunnorm),uow_loadtest$eleven_h)
#testing pperformance of MSE
MSE(exp(uow_load_pred_Narxunnorm),uow_loadtest$eleven_h)
#testing pperformance of MAPE
MAPE(exp(uow_load_pred_Narxunnorm),uow_loadtest$eleven_h)
#correlation between predicted and actual values
cor(uow_load_pred_Narxunnorm,uow_loadtest$eleven_h)

#generate eleventh hour plot
par(mfrow=c(1,1))
plot(uow_loadtest$eleven_h,uow_load_pred_Narxunnorm,col='green',main='unnormalized Prediction Graph AR',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend("bottomright",legend='NN',pch=18,col='green',byt='n')
uow_load_finalNXres<-cbind(uow_loadtest,uow_load_pred_Narxunnorm)
uow_load_finalNXres
plot(uow_loadtest$eleven_h,ylab="Predicted vs Expected AR",type='l',col="red")
par(new=TRUE)
plot(uow_load_pred_Narxunnorm,ylab='',yaxt='n',type='l',col='green',main='Predicted vs Expected val AR')
legend("topleft",
       c("Expected","Predicted"),
       fill=c("red","green")
)
#calculate accuracy
NXpredict=uow_load_nraxmodel_res*abs(diff(range(uow_load_testnorm$eleven_h)))+min(uow_load_testnorm$eleven_h)
NXactual=uow_load_testnorm$eleven_h*abs(diff(range(uow_load_testnorm$eleven_h)))+min(uow_load_testnorm$eleven_h)
NXcompare=data.frame(NXpredict,NXactual)
NXdeviation=((NXactual-NXpredict)/NXactual)
NXdeviation
is.na(NXdeviation)<-sapply(NXdeviation,is.infinite)

NXdeviation
NXdev_NAomit<-na.omit(NXdeviation)
NXdev_NAomit
NXcompare= data.frame(NXpredict,NXactual,NXdeviation)
NXaccuracy=1-abs(mean(NXdev_NAomit))
NXaccuracy