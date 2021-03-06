#### REQUIRED PACKAGES ####

list.of.packages <- c( "data.table" , "glmnet","randomForest", "xgboost","pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(gbm)
library(glmnet)	 
library(data.table)
library(randomForest)
library(xgboost)
library(pROC)
library(lubridate)
library(ggplot2)


#### PARAMETERS ####

options(scipen=999)
max_lag=65
shift=500


####### FUNCTIONS ###########

get_matrix<-function(data_frame){
  for (i in 1:ncol(data_frame)){
    if(is.factor(data_frame[,i])){
      data_frame[,i]<-as.numeric(as.character(data_frame[,i]))
    } else if (is.character(data_frame[,i])) {
      data_frame[,i]<-as.numeric(data_frame[,i])
    } 
  }
  return(as.matrix(data_frame))
}

remove_outliers <- function(x) {
  y <- x
  y[x < -3] <- median(x)
  y[x > 3] <- median(x)
  y
}



forecast_glmnet3<-function(dates_pred,X,Y_mcc,cvfit,s){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y_mcc)
  Y_actual<-Y_mcc[!(Y_mcc$date %in% dates_pred), ]
  for (n in 1:length(dates_pred)){
    dates_test_i<-dates_pred[n]
    X_test_lagged<-X_lagged_create2(dates_test_i,Y_actual,max_lag)
    X_test_lagged[,1]<-NULL
    X_test<-X[match(dates_test_i,X[,1]),-1]
    X_test_<-as.data.frame(t(matrix(rep(X_test,nrow(X_test_lagged)),ncol=nrow(X_test_lagged))))
    colnames(X_test_)<-colnames(X_test)
    X_test_mx<-data.matrix(cbind(X_test_,X_test_lagged))
    y_pred<-predict(cvfit,X_test_mx,s=s,type = "link")
    Y_new<-data.frame(rep(dates_test_i,length(y_pred)),Y_actual$variable[1:length(y_pred)],y_pred)
    colnames(Y_new)<-colnames(Y_actual)
    Y_actual<-rbind(Y_actual,Y_new)
  }
  return(Y_actual)
}
X_lagged_create2<-function(actual_date,Y_melt,max_lag){
  X_lagged<-Y_melt[Y_melt$date>=(min(actual_date)-max_lag-1),]
  X_lagged<-merge(lag_dates,X_lagged,by.x="lag_date",by.y="date")
  X_lagged<-dcast(X_lagged,actual_date+variable.y~variable.x,value.var="value")
  X_lagged<-X_lagged[X_lagged[,1] %in% actual_date,]
  X_lagged[,1]<-NULL
  return(X_lagged)
}

get_lagged_dates<-function(max_lag=35, actual_dates=X$date){
  lag_dates<-data.frame(actual_date=actual_dates)
  for (i in 1:max_lag){
    lag_dates$lag_date_calendar<-lag_dates$actual_date-i
    colnames(lag_dates)[ncol(lag_dates)]<-paste0("lag_",i,"d")
  }
  lag_dates_melted<-melt(lag_dates,id="actual_date")
  colnames(lag_dates_melted)[3]<-"lag_date"
  return(lag_dates_melted)
}

#### DATA LOAD ####

source("data_prep.R")
holidays_dt<-fread("data/holidays_list.csv")
holidays_list<-as.Date(holidays_dt$holidays_list,format="%d.%m.%y")


#### DATASET ####

mcc_by_days<-transactions[direction=="outbound",.(amount=sum(amount)),by=.(date,mcc_code)]
mcc_by_days<-dcast(mcc_by_days,date~mcc_code,value.var = "amount")
mcc_by_days<-melt(mcc_by_days,id="date")
mcc_by_days[is.na(mcc_by_days)]<-0
mcc_by_days<-as.data.frame(mcc_by_days)
mcc_by_days$date<-as.Date(mcc_by_days$date,fprmat="%Y-%m-%d")
Y<-mcc_by_days
Y$value<-log(-mcc_by_days$value+shift)
dates<-c(unique(Y$date),seq.Date(max(Y$date)+1,by="day",length.out = 30))
lag_dates<-get_lagged_dates(max_lag,dates)
X_lagged<-Y
X_lagged<-merge(lag_dates,X_lagged,by.x="lag_date",by.y="date")
X_lagged<-dcast(X_lagged,actual_date+variable.y~variable.x,value.var="value")
X_lagged[is.na(X_lagged)]<-0
X<-data.frame(date=dates,is_holiday=as.integer(dates %in% holidays_list))
X$month<-month(X$date)
X$day<-day(X$date)
X$week<-week(X$date)
X$weekday<-as.factor(wday(X$date))
X<-with(X,data.frame(X[,!(colnames(X) %in% c("weekday"))],
                   model.matrix(~weekday-1,X)))
X_total<-merge(X,X_lagged,by.x="date",by.y="actual_date")
X_total<-merge(X_total,Y,by.x=c("date","variable.y"),by.y=c("date","variable"),all.x = T)
colnames(X_total)[2]<-"mcc_code"


#### LOOP ####

dates_train<-dates_cleaned[dates_cleaned<=as.Date("2015/9/30")] 
dates_test<-dates_cleaned[dates_cleaned>as.Date("2015/9/30")]
dates_cleaned<-unique(Y$date)
dates_pred<-seq.Date(max(dates_cleaned)+1,by="day",length.out = 30)
Y_pred<-data.frame()
for (mcc in unique(X_total$mcc_code)){
  Y_mcc<-Y[Y$variable==mcc,]
  X_mcc<-X_total[X_total$mcc_code==mcc,]
  X_train<-X_total[X_total$date %in% dates_train,]
  X_train$date<-NULL
  X_train$value<-NULL
  X_train$mcc_code<-NULL
  y_train<-X_total[X_total$date %in% dates_train,"value"]
  X_train_mx<-get_matrix(X_train)
  cvfit <- cv.glmnet(X_train_mx, y_train,family = "gaussian",alpha=1,nfolds = 5,type.measure = "mse")
  Y_pred_mcc<-forecast_glmnet3(dates_test,X,Y_mcc,cvfit,s=cvfit$lambda.min)
  Y_pred<-rbind(Y_pred,Y_pred_mcc)
}

Y_pred<-Y_pred[Y_pred$date %in% dates_pred,]
Y_pred$actual_value<-exp(Y_pred$value)
Y_pred$day<-as.integer(Y_pred$date-min(Y_mcc$date))
submission<-Y_pred[Y_pred$day>=457,]
submission<-submission[,c(2,5,4)]
colnames(submission)<-c("mcc_code",	"day",	"volume")
write.csv(submission, 'output/task2/submission.csv', row.names = F,quote=FALSE)

Y_pred$sample<-ifelse(Y_pred$date %in% dates_train, "actual", "predicted")
Y_act<-Y[Y$date %in% dates_test,]
Y_act$sample<-"actual"
visual<-rbind(Y_pred,Y_act)
for (var in unique(visual$variable)){
  ggplot(visual[visual$variable==var,],aes(x=date,y=value,color=sample))+
  geom_point() +
  geom_smooth()
}

