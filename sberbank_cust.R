#### REQUIRED PACKAGES ####

list.of.packages <- c( "data.table" , "glmnet","randomForest", "xgboost","pROC","FeatureHashing")
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
library(FeatureHashing)

#### PARAMETERS ####

options(scipen=999)
shift=1


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




X_lagged_create<-function(actual_date,Y_melt,max_lag){
  X_lagged<-Y_melt[Y_melt$date>=(min(actual_date)-max_lag-1),]
  X_lagged<-merge(lag_dates,X_lagged,by.x="lag_date",by.y="date")
  X_lagged<-dcast(X_lagged,actual_date+variable.y~variable.x,value.var="value")
  X_lagged<-X_lagged[X_lagged[,1] %in% actual_date,]
  X_lagged[,1]<-NULL
  return(X_lagged)
}


#### DATA LOAD ####

source("data_prep.R")
gender<-fread("data/customers_gender_train.csv")
holidays_dt<-fread("data/holidays_list.csv")
holidays_list<-as.Date(holidays_dt$holidays_list,format="%d.%m.%y")


#### DATASET ####

all_cust <-unique(transactions$customer_id[!(transactions$customer_id %in% gender$customer_id)])
all_mcc <-unique(transactions$mcc_code)
mcc_by_days<-transactions[direction=="outbound",.(amount=sum(amount)),by=.(date,mcc_code,customer_id)]
mcc_by_days$date<-as.Date(mcc_by_days$date,format="%Y-%m-%d")
mcc_by_days$year_month<-format(mcc_by_days$date,"%Y-%m-%01")
mcc_by_days$year<-year(mcc_by_days$date)
mcc_by_days$month<-month(mcc_by_days$date)

dates<-c(unique(mcc_by_days$date),seq.Date(max(mcc_by_days$date)+1,by="day",length.out = 30))
X<-data.table(date=dates,is_holiday=as.integer(dates %in% holidays_list))
X$year_month<-format(X$date,"%Y-%m-%01")
X$year<-year(X$date)
X$month<-month(X$date)
X$day<-day(X$date)
X$week<-week(X$date)
X$weekday<-as.factor(wday(X$date))
X$weekend<-as.integer(X$weekday %in% c(7,1))
calendar_stats<-X[,.(days=max(day),holidays=sum(is_holiday),work_days=(max(day)-sum(is_holiday)-sum(weekend))),by=.(year,month,year_month)]

grid<-as.data.table(expand.grid(year_month=calendar_stats$year_month,mcc_code=all_mcc,customer_id=all_cust))
total<-merge(grid,mcc_by_days[,.(amount=sum(amount)),by=.(year_month,mcc_code,customer_id)],by=c("year_month","mcc_code","customer_id"),all.x=TRUE)
total<-merge(total,calendar_stats,by="year_month")
total$year_month<-as.Date(total$year_month)
for (month_shift in c(1,2, 3)){
  total_shift<-total
  total_shift$year_month<-total_shift$year_month+months(month_shift)
  total<-merge(total,total_shift[,c("year_month","mcc_code","customer_id","amount"),with=F],by=c("year_month","mcc_code","customer_id"),all.x=TRUE)
  names(total)[names(total)=="amount.x"] <- "amount"
  names(total)[names(total)=="amount.y"] <- paste0("amount_lag_",month_shift,"m")
}
total[is.na(total)]<-0
total$amount<-log(-total$amount+1)
total$amount_lag_1m<-log(-total$amount_lag_1m+1)
total$amount_lag_2m<-log(-total$amount_lag_2m+1)
total$amount_lag_3m<-log(-total$amount_lag_3m+1)
total$amount_lag_12m<-log(-total$amount_lag_12m+1)
predictor_cols <- names(total)[!(names(total) %in% c("year_month","amount"))]
X_train<-total[year_month!="2015-11-01",]
X_test<-total[year_month=="2015-11-01",]
train_hashed <- hashed.model.matrix(~., data=X_train[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
train_hashed <- as(train_hashed, "dgCMatrix")
test_hashed <- hashed.model.matrix(~., data=X_test[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
test_hashed <- as(test_hashed, "dgCMatrix")
glmnetModel <- cv.glmnet(train_hashed, X_train$amount, nfolds = 5,
                         family = "gaussian",alpha=1,type.measure = "mse")

y_pred <- predict(glmnetModel, test_hashed, s="lambda.min")
submission<-data.table(X_test[,c("customer_id","mcc_code"),with=F],volume=(exp(y_pred)-1))
colnames(submission)<-c("customer_id",	"mcc_code",	"volume")
write.csv(submission, 'output/task3/submission.csv', row.names = F,quote=FALSE)
