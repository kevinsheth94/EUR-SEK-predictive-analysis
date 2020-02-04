library(e1071)
library(tseries)
dataset <- read.csv("EURSEK_data.csv",TRUE)
  training <- dataset
  testing <- dataset
  training1<-training
  SVM<-svm(EUR_SEKreturn~Volatility,data=training1,type="eps-regression",kernel="radial")
  SVMpred<-predict(SVM)
  errortrain<-training$EUR_SEKreturn-SVMpred
  predict<-c()
  predict[1]<-predict(SVM,new=testing[1,])
  error<-c()
  error[1]<-testing$EUR_SEKreturn[1]-predict[1]
  j<-2
  for( i in 1:(nrow(testing)-1)){
    training1<-rbind(training1,testing[i,])
    SVM<-svm(EUR_SEKreturn~Volatility,data=training1,type="eps-regression",kernel="radial")
    predict[j]<-predict(SVM,new=testing[j,])
    error[j]<-testing$EUR_SEKreturn[j]-predict[j]
    j<-j+1
  }
  res<-c(errortrain,error) #residuals vector
  r<-garch(res,order=c(0,1)) # ARCH model
  pred_sd<-predict(r) # returns +/- the conditional standard deviation predictions
  #scale response (return) variable
  newdata<-rbind(training,testing)
  new_return<-newdata$EUR_SEKreturn[-1]/pred_sd[-c(1),1]
  #modelling new model
  newscaleddata<-rbind(training[-1,],testing)
  newscaleddata$EUR_SEKreturn<-new_return
  #split into 70:30
  integer1<-as.integer(nrow(newscaleddata)*0.7)
  n_newdata<-nrow(newscaleddata)
  newtraining<-newscaleddata[1:integer1,]
  newtesting<-newscaleddata[(integer1+1):n_newdata,]
  dim(testing)
  estimatedsd<-pred_sd[(integer1+1):n_newdata,1]
  mylist<-list(predict,error,estimatedsd)
  NewDataFrame <- do.call("cbind", mylist)
  #print(mylist)
  dat<-list(testing[2])
  dat1<-list(predict)
  dat2<-list(testing[3])
  dat3<-list(error)
  datframe <- do.call("cbind", dat)
  PredictedData <- do.call("cbind", dat1)
  datframe2 <- do.call("cbind", dat2)
  PredictedVolatility <- do.call("cbind", dat3)
  plot(PredictedData,ylim=c(1,1.5),type='l',col='red')
  lines(datframe)
  plot(PredictedVolatility,type='l',col='red')
  lines(datframe2)
  #print(error)
  rmse_svm<-c()
  for( i in 1:523)
  {rmse_svm[i]=sqrt( mean( (error[i])^2 , na.rm = TRUE ) )}
  print(rmse_svm)
  dat4 <- list(rmse_svm)
  rmseframe <- do.call("cbind", dat4)
  #print(dat)
  write.csv(NewDataFrame,'EURSEK_output_data.csv',row.names=T)
  write.csv(datframe1,'EURSEK_predicted_output_data.csv.csv',row.names=T)
  write.csv(rmseframe,'EURSEK_output_data_rmse.csv.csv',row.names=T)

  