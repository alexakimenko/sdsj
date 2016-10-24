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

forecast_glmnet<-function(dates_pred,X,Y){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y)
  Y_actual<-Y[!(Y$Date %in% dates_pred), ]
  Y_pred<-rbind(Y_actual,new_rows)
  for (n in 1:length(dates_pred)){
    for (mcc_code in colnames(Y_pred)[-1]){
      dates_test_i<-dates_pred[n]
      fit_glmnet<-get(paste0(mcc_code,"_glmnet"))
      X_test<-X[match(dates_test_i,X[,1]),-1]
      X_test_lagged<-X_lagged_create(mcc_code,dates_test_i,Y_pred)
      X_test<-cbind(X_test,X_test_lagged)
      y_pred<-predict(fit_glmnet,X_test)
      Y_pred[Y_pred$Date==dates_test_i,mcc_code]<-y_pred
    }
  }
  return(tail(Y_pred,length(dates_pred)))
}

forecast_glmnet2<-function(dates_pred,X,Y,fit_glmnet){ #Y and X need to have dates as 1st var
  new_rows<-data.frame(dates_pred,matrix(c(rep.int(NA,length(dates_pred)*(ncol(Y)-1))),nrow=length(dates_pred),ncol=ncol(Y)-1))
  colnames(new_rows)<-colnames(Y)
  new_rows_melt<-melt(new_rows,id="date")
  Y_actual<-Y[!(Y$date %in% dates_pred), ]
  Y_actual_melt<-melt(Y_actual,id="date")
  Y_pred<-rbind(Y_actual_melt,new_rows_melt)
  for (n in 1:length(dates_pred)){
    dates_test_i<-dates_pred[n]
    X_test_lagged<-X_lagged_create2(dates_test_i,Y_melt=Y_pred)
    colnames(X_test_lagged)[1]<-"mcc_code"
    X_test_lagged_<-with(X_test_lagged,data.frame(X_test_lagged[,!(colnames(X_test_lagged) %in% c("mcc_code"))],
                         model.matrix(~mcc_code-1,X_test_lagged)))
    X_test<-X[match(dates_test_i,X[,1]),-1]
    X_test_<-as.data.frame(t(matrix(rep(X_test,nrow(X_test_lagged_)),ncol=nrow(X_test_lagged_))))
    colnames(X_test_)<-colnames(X_test)
    X_test_mx<-data.matrix(cbind(X_test_,X_test_lagged_))
    y_pred<-predict(fit_glmnet,X_test_mx,s=0,type = "link")
    Y_pred[Y_pred$date==dates_test_i,3]<-y_pred
  }
  return(Y_pred)
}

X_lagged_create2<-function(actual_date,Y_melt){
  X_lagged<-Y_melt
  X_lagged<-merge(lag_dates,X_lagged,by.x="lag_date",by.y="date")
  X_lagged<-dcast(X_lagged,actual_date+variable.y~variable.x,value.var="value")
  X_lagged<-X_lagged[X_lagged[,1] %in% actual_date,]
  X_lagged[,1]<-NULL
  return(X_lagged)
}

X_lagged_create<-function(mcc_code,actual_date,Y){
  X_lagged<-data.frame(actual_date=Y[,1],value=Y[,mcc_code])
  X_lagged<-merge(lag_dates,X_lagged,by.x="lag_date",by.y="actual_date")
  X_lagged<-dcast(X_lagged,actual_date~variable,mean)
  X_lagged<-X_lagged[match(actual_date,X_lagged[,1]),]
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

lag_dates<-get_lagged_dates(65,X$date)
mcc_by_days<-transactions[direction=="outbound",.(amount=sum(amount)),by=.(date,mcc_code)]
mcc_by_days<-dcast(mcc_by_days,date~mcc_code,value.var = "amount")
mcc_by_days[is.na(mcc_by_days)]<-0
mcc_by_days$date<-as.Date(mcc_by_days$date, format="%Y-%m-%d")
stand_df<-data.frame()
mcc_by_days<-as.data.frame(mcc_by_days)
Y<-mcc_by_days
shift=500
for (i in 2:ncol(mcc_by_days)){
  Y[,i]<-log(-mcc_by_days[,i]+shift)
  st_i<-data.frame(col_name=colnames(Y)[i],mean=mean(Y[,i]),sd=sd(Y[,i]))
  Y[,i]<-(Y[,i]-mean(Y[,i]))/sd(Y[,i])
  stand_df<-rbind(stand_df,st_i)
}
Y_melt<-melt(Y,id="date")
saveRDS(stand_df, "output/task2/stand_df.rds")
dates<-c(Y[,1],seq.Date(max(Y[,1])+1,by="day",length.out = 30))
X<-data.frame(date=dates,is_holiday=as.integer(dates %in% holidays_list))
X$month<-month(X$date)
X$day<-day(X$date)
X$week<-week(X$date)
X$weekday<-as.factor(wday(X$date))
X<-with(X,data.frame(X[,!(colnames(X) %in% c("weekday"))],
                   model.matrix(~weekday-1,X)))


#### CLEANING & SAMPLING ####

dates_cleaned<-Y[,1]
dates_train<-dates_cleaned[dates_cleaned<=as.Date("2015/9/30")] 
dates_test<-dates_cleaned[dates_cleaned>as.Date("2015/9/30")]

X_train_total<-data.frame()
X_test_total<-data.frame()
y_train_total<-data.frame()
y_test_total<-data.frame()
for (mcc_code in colnames(Y)[-1]){
  X_train_lagged<-X_lagged_create(mcc_code,dates_train,Y)
  X_test_lagged<-X_lagged_create(mcc_code,dates_test,Y)
  X_train<-X[match(dates_train,X[,1]),-1]
  X_test<-X[match(dates_test,X[,1]),-1]
  X_train<-cbind(X_train,X_train_lagged)
  X_test<-cbind(X_test,X_test_lagged)
  X_train$mcc_code<-mcc_code
  X_test$mcc_code<-mcc_code
  y_train<-data.frame(y_train=Y[match(dates_train,Y[,1]),mcc_code])
  y_test<-data.frame(y_test=Y[match(dates_test,Y[,1]),mcc_code])
  y_train$mcc_code<-mcc_code
  y_test$mcc_code<-mcc_code
  X_train_total<-rbind(X_train_total,X_train)
  X_test_total<-rbind(X_test_total,X_test)
  y_train_total<-rbind(y_train_total,y_train)
  y_test_total<-rbind(y_test_total,y_test)
}

X_train_total[is.na(X_train_total)]<-0
X_test_total[is.na(X_test_total)]<-0

saveRDS(X_train_total,"output/task2/X_train_total.rds")
saveRDS(X_test_total,"output/task2/X_test_total.rds")
saveRDS(y_train_total,"output/task2/y_train_total.rds")
saveRDS(y_test_total,"output/task2/y_test_total.rds")
X_train_total2<-with(X_train_total,data.frame(X_train_total[,!(colnames(X_train_total) %in% c("mcc_code"))],
                                           model.matrix(~mcc_code-1,X_train_total)))
X_test_total2<-with(X_test_total,data.frame(X_test_total[,!(colnames(X_test_total) %in% c("mcc_code"))],
                     model.matrix(~mcc_code-1,X_test_total)))
y_train_total2<-y_train_total$y_train
y_test_total2<-y_test_total$y_test
saveRDS(X_train_total2,"output/task2/X_train_total2.rds")
saveRDS(X_test_total2,"output/task2/X_test_total2.rds")
saveRDS(y_train_total2,"output/task2/y_train_total2.rds")
saveRDS(y_test_total2,"output/task2/y_test_total2.rds")


#### CHALLENGER MODELS ####

#### GLMNET ####

X_train_mx<-get_matrix(X_train_total2)
X_test_mx<-get_matrix(X_test_total2)
y_train<-y_train_total2
y_test<-y_test_total2
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



#### RANDOM FOREST ####

fit_rf <- randomForest(as.data.frame(X_train), as.factor(y_train), ntree=100, importance=TRUE)
imp_rf<-data.frame(importance(fit_rf))
imp_rf<-imp_rf[order(-imp_rf$MeanDecreaseGini),]
for (x in seq(60,150,by=30)){
  for (n in seq(100,800,by=200)){
    top_pred<-rownames(imp_rf)[1:x]
    fit_rf <- randomForest(as.data.frame(X_train)[,top_pred], as.factor(y_train), ntree=n, importance=TRUE)
    y_train_pred<-predict(fit_rf,as.data.frame(X_train),type="prob")
    y_test_pred<-predict(fit_rf,as.data.frame(X_test),type="prob")
    out_i<-data.frame(model=paste0("randomForest (n_predictors=",x,"ntree=",n,")"),
                      auc_train=auc(y_train, y_train_pred[,2]),
                      auc_test=auc(y_test, y_test_pred[,2]))
    summary<-rbind(summary,out_i)
  }
} 
summary[order(-summary$auc_test),]


#### XGBOOST ####

min_c_w<-1/sqrt(sum(y_train)/length(y_train))
X_train<-get_matrix(train)
X_test<-get_matrix(test)
y_train<-y==1
xgbMatrix <- xgb.DMatrix(data=X_train, 
                         label = y_train)
for (e in c(0.01, 0.05, 0.1, 0.02)){
  for (max_depth in c(2, 4, 6, 8)){
    for (min_c_w in c(35,60)){
      col_bytree=0.3
      param1 <- list("objective" = "binary:logistic", 
                   "eval_metric" = "auc", 
                   "max_depth" = max_depth, 
                   "eta" = e,
                   "min_child_weight" = min_c_w,
                   "colsample_bytree"=col_bytree)
      cv_xgboost <- xgb.cv(data=xgbMatrix,
                         nfold = 5, maximize = FALSE, 
                         nrounds = 100, params=param1)
      out_i<-data.frame(model=paste0("XGBoost (eta=",e,", max_depth=",max_depth,", colsample_bytree=",col_bytree,
                                     ", min_child_weight=",min_c_w,")"),
                      error_train=tail(cv_xgboost$train.auc.mean,1),
                      error_test=tail(cv_xgboost$test.auc.mean,1),
                      diff=tail(cv_xgboost$train.auc.mean,1)-tail(cv_xgboost$test.auc.mean,1))
      summary<-rbind(summary,out_i)
    }
  }  
}

summary[order(-summary$error_test),]
summary$diff<-summary$error_train-summary$error_test
summary<-summary[1:60,]
#### SUBMISSION ####

dates_pred<-seq.Date(max(Y[,1])+1,by="day",length.out = 30)
Y_pred<-forecast_glmnet2(dates_pred,X,Y,fit_glmnet)
Y_pred<-merge(Y_pred,stand_df,by.x="variable",by.y="col_name")
Y_pred$actual_value<-exp(Y_pred$value*Y_pred$sd+Y_pred$mean)
Y_pred$actual_value<-ifelse(Y_pred$actual_value==1,0,Y_pred$actual_value)
Y_pred$day<-as.integer(Y_pred$date-min(Y_pred$date))
submission<-Y_pred[Y_pred$day>=457,]
submission<-submission[,c(1,7,6)]
colnames(submission)<-c("mcc_code",	"day",	"volume")
write.csv(submission, 'output/task2/submission.csv', row.names = F,quote=FALSE)

