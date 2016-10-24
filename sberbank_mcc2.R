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


forecast_glmnet2<-function(dates_pred,X,Y,fit_glmnet,s){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y)
  Y_actual<-Y[!(Y$date %in% dates_pred), ]
  for (n in 1:length(dates_pred)){
    dates_test_i<-dates_pred[n]
    X_test_lagged<-X_lagged_create2(dates_test_i,Y_actual,max_lag)
    colnames(X_test_lagged)[1]<-"mcc_code"
    X_test_lagged_<-with(X_test_lagged,data.frame(X_test_lagged[,!(colnames(X_test_lagged) %in% c("mcc_code"))],
                         model.matrix(~mcc_code-1,X_test_lagged)))
    X_test<-X[match(dates_test_i,X[,1]),-1]
    X_test_<-as.data.frame(t(matrix(rep(X_test,nrow(X_test_lagged_)),ncol=nrow(X_test_lagged_))))
    colnames(X_test_)<-colnames(X_test)
    X_test_mx<-data.matrix(cbind(X_test_,X_test_lagged_))
    y_pred<-predict(fit_glmnet,X_test_mx,s=s,type = "link")
    Y_new<-data.frame(rep(dates_test_i,length(y_pred)),X_test_lagged$mcc_code,y_pred)
    colnames(Y_new)<-colnames(Y_actual)
    Y_actual<-rbind(Y_actual,Y_new)
  }
  return(Y_actual)
}
forecast_xgb<-function(dates_pred,X,Y,fit_xgboost){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y)
  Y_actual<-Y[!(Y$date %in% dates_pred), ]
  for (n in 1:length(dates_pred)){
    dates_test_i<-dates_pred[n]
    X_test_lagged<-X_lagged_create2(dates_test_i,Y_actual,max_lag)
    colnames(X_test_lagged)[1]<-"mcc_code"
    X_test_lagged_<-with(X_test_lagged,data.frame(X_test_lagged[,!(colnames(X_test_lagged) %in% c("mcc_code"))],
                                                  model.matrix(~mcc_code-1,X_test_lagged)))
    X_test<-X[match(dates_test_i,X[,1]),-1]
    X_test_<-as.data.frame(t(matrix(rep(X_test,nrow(X_test_lagged_)),ncol=nrow(X_test_lagged_))))
    colnames(X_test_)<-colnames(X_test)
    X_test_mx<-data.matrix(cbind(X_test_,X_test_lagged_))
    y_pred<-predict(fit_xgboost,X_test_mx)
    Y_new<-data.frame(rep(dates_test_i,length(y_pred)),X_test_lagged$mcc_code,y_pred)
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
X_total<-with(X_total,data.frame(X_total[,!(colnames(X_total) %in% c("variable.y"))],
                           model.matrix(~variable.y-1,X_total)))


#### CLEANING & SAMPLING ####

dates_cleaned<-unique(Y$date)
dates_train<-dates_cleaned[dates_cleaned<=as.Date("2015/9/30")] 
dates_test<-dates_cleaned[dates_cleaned>as.Date("2015/9/30")]
dates_pred<-seq.Date(max(dates_cleaned)+1,by="day",length.out = 30)
X_train<-X_total[X_total$date %in% dates_train,]
X_test<-X_total[X_total$date %in% dates_test,]
X_train$date<-NULL
X_test$date<-NULL
X_train$value<-NULL
X_test$value<-NULL
y_train<-X_total[X_total$date %in% dates_train,"value"]
y_test<-X_total[X_total$date %in% dates_test,"value"]
saveRDS(X_train,"output/task2/X_train.rds")
saveRDS(X_test,"output/task2/X_test.rds")
saveRDS(y_train,"output/task2/y_train.rds")
saveRDS(y_test,"output/task2/y_test.rds")


#### CHALLENGER MODELS ####

#### GLMNET ####

X_train_mx<-get_matrix(X_train)
X_test_mx<-get_matrix(X_test)
summary<-data.table()
for (s in seq(0,1,by=0.05)){
  for (a in seq(0,1,by=0.1)){
    fit_glmnet <- glmnet(X_train_mx, y_train,alpha = a,family = "gaussian")
    y_train_pred<-predict(fit_glmnet,X_train_mx,s=s,type="link")
    y_test_pred<-predict(fit_glmnet,X_test_mx,s=s,type = "link")
    out_i<-data.frame(model=paste0("penalized regresson (alpha=",a,"lambda=",s,")"),
                                   rmse_train=sqrt(mean((y_train-y_train_pred)^2)) ,
                                   rmse_test=sqrt(mean((y_test-y_test_pred)^2)) )
    summary<-rbind(summary,out_i)
  }
}
head(summary[order(summary$rmse_test),],100)

X_train<-X_total[!(X_total$date %in% dates_pred),]
y_train<-X_total[!(X_total$date %in% dates_pred),"value"]
X_train$date<-NULL
X_train$value<-NULL
X_train_mx<-get_matrix(X_train)
fit_glmnet <- glmnet(X_train_mx, y_train,alpha = 0,family = "gaussian")
Y_pred<-forecast_glmnet2(dates_pred,X,Y,fit_glmnet,s=0)
Y_pred<-Y_pred[Y_pred$date %in% dates_test,]
Y_act<-Y[Y$date %in% dates_test,]
test<-as.data.table(merge(Y_pred,Y_act,by=c("date","variable")))
sqrt(mean((test$value.y-test$value.x)^2))
t<-test[,.(sqrt(mean((value.y-value.x)^2))),by=.(variable)]
head(t[order(V1)],10)
test_i<-test[test$variable=="5541",]
test_i<-Y[Y$variable=="5977",]
plot(x=test_i$date,y=test_i$value.y)
lines(x=test_i$date,y=test_i$value.x)

#### XGBOOST ####


X_train<-get_matrix(X_train)
X_test<-get_matrix(X_test)
xgbMatrix <- xgb.DMatrix(data=X_train, 
                         label = y_train)
for (e in c(0.01, 0.05, 0.1, 0.02)){
  for (max_depth in c(2, 4, 6, 8)){
    for (min_c_w in c(2,35,60)){
      col_bytree=0.3
      param1 <- list("objective" = "reg:linear", 
                   "eval_metric" = "rmse", 
                   "max_depth" = max_depth, 
                   "eta" = e,
                   "min_child_weight" = min_c_w,
                   "colsample_bytree"=col_bytree)
      cv_xgboost <- xgb.cv(data=xgbMatrix,
                         nfold = 5, maximize = FALSE, 
                         nrounds = 100, params=param1)
      out_i<-data.frame(model=paste0("XGBoost (eta=",e,", max_depth=",max_depth,", colsample_bytree=",col_bytree,
                                     ", min_child_weight=",min_c_w,")"),
                      rmse_train=tail(cv_xgboost$train.rmse.mean,1),
                      rmse_test=tail(cv_xgboost$test.rmse.mean,1))
      summary<-rbind(summary,out_i)
    }
  }  
}

summary[order(summary$rmse_test),]
summary$diff<-summary$error_train-summary$error_test
summary<-summary[1:60,]

X_train<-X_total[!(X_total$date %in% dates_pred),]
y_train<-X_total[!(X_total$date %in% dates_pred),"value"]
X_train$date<-NULL
X_train$value<-NULL
X_train_mx<-get_matrix(X_train)
xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
                         label = y_train)
param1 <- list("objective" = "reg:linear", 
               "eval_metric" = "rmse", 
               "max_depth" = 6, 
               "eta" = 0.05,
               "min_child_weight" = 35,
               "colsample_bytree"=0.3)
fit_xgboost <- xgb.train(data=xgbMatrix,
                         maximize = FALSE, 
                         nrounds = 100, params=param1)
Y_pred<-forecast_xgb(dates_pred,X,Y,fit_xgboost)

#### SUBMISSION ####


Y_pred<-Y_pred[Y_pred$date %in% dates_pred,]
Y_pred$actual_value<-exp(Y_pred$value)
Y_pred$day<-as.integer(Y_pred$date-min(Y$date))
Y_pred<-Y_pred[order(Y_pred$variable),]
submission<-Y_pred[Y_pred$day>=457,]
submission<-submission[,c(2,5,4)]
colnames(submission)<-c("mcc_code",	"day",	"volume")
write.csv(submission, 'output/task2/submission.csv', row.names = F,quote=FALSE)

