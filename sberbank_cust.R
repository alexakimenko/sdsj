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




#### DATA LOAD ####

transactions<-fread("output/transactions.csv")
trans_w_g<-fread("output/task1/trans_w_g.csv")
gender<-fread("data/customers_gender_train.csv")
tr_mcc_codes<-fread("data/tr_mcc_codes.csv")
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
transactions$date<-as.Date(transactions$date,format="%Y-%m-%d")
transactions$year_month<-format(transactions$date,"%Y-%m-%01")
cm_account_margin<-transactions[,.(amount=sum(amount)),by=.(year_month,customer_id)]
cm_account_margin$year_month<-as.Date(cm_account_margin$year_month,format="%Y-%m-%d")
cm_account_margin$year_month<-cm_account_margin$year_month+months(1)
cm_account_margin$margin_amnt<-(cm_account_margin$amount-mean(cm_account_margin$amount))/sd(cm_account_margin$amount)
cm_account_margin$amount<-NULL

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
for (month_shift in c(1,2,3,12)){
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
total<-merge(total,cm_account_margin,by=c("year_month","customer_id"),all.x=T)
total$margin_amnt[is.na(total$margin_amnt)]<-0
total$amount_l2m_avg<-(total$amount_lag_1m+total$amount_lag_2m)/2
total$amount_l3m_avg<-(total$amount_lag_1m+total$amount_lag_2m+total$amount_lag_3m)/3
total$customer_id<-as.factor(total$customer_id)
total$mcc_code<-as.factor(total$mcc_code)
predictor_cols <- names(total)[!(names(total) %in% c("year_month","amount"))]
X_train<-total[year_month!="2015-11-01"& year_month!="2015-01-01",]
X_test<-total[year_month=="2015-11-01",]
train_hashed <- hashed.model.matrix(~., data=X_train[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
train_hashed <- as(train_hashed, "dgCMatrix")
test_hashed <- hashed.model.matrix(~., data=X_test[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
test_hashed <- as(test_hashed, "dgCMatrix")
# glmnetModel <- cv.glmnet(train_hashed, X_train$amount, nfolds = 5,
#                          family = "gaussian",alpha=1,type.measure = "mse")
# y_pred <- predict(glmnetModel, test_hashed, s="lambda.min")

summary<-data.table()
for (e in c(0.01,  0.1)){ ### запустить от сюда!
  for (max_depth in c(8, 10,12)){
    for (min_c_w in c(30)){
      col_bytree=0.3
      param1 <- list("objective" = "reg:linear", 
                     "eval_metric" = "rmse", 
                     "max_depth" = max_depth, 
                     "eta" = e,
                     "min_child_weight" = min_c_w,
                     "colsample_bytree"=col_bytree)
      cv_xgboost <- xgb.cv(data=train_hashed,label = X_train$amount,
                           nfold = 5, maximize = FALSE, 
                           nrounds = 100, params=param1)
      out_i<-data.frame(model=paste0("XGBoost (eta=",e,", max_depth=",max_depth,", colsample_bytree=",col_bytree,
                                     ", min_child_weight=",min_c_w,")"),
                        error_train=tail(cv_xgboost$train.rmse.mean,1),
                        error_test=tail(cv_xgboost$test.rmse.mean,1),
                        diff=tail(cv_xgboost$train.rmse.mean,1)-tail(cv_xgboost$test.rmse.mean,1))
      summary<-rbind(summary,out_i)
    }
  }  
}
summary[order(summary$error_test),]

param1 <- list("objective" = "reg:linear", 
               "eval_metric" = "rmse", 
               "max_depth" = 8, 
               "eta" = 0.1,
               "min_child_weight" = 30,
               "colsample_bytree"=0.3)
cv_xgboost <- xgboost(data=train_hashed,label = X_train$amount,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1)
y_pred <- predict(cv_xgboost, test_hashed)
submission<-data.table(X_test[,c("customer_id","mcc_code"),with=F],volume=(exp(y_pred)-1))
colnames(submission)<-c("customer_id",	"mcc_code",	"volume")
write.csv(submission, 'output/task3/submission_xgb_md8.csv', row.names = F,quote=FALSE)

param1 <- list("objective" = "reg:linear", 
               "eval_metric" = "rmse", 
               "max_depth" = 10, 
               "eta" = 0.1,
               "min_child_weight" = 30,
               "colsample_bytree"=0.3)
cv_xgboost <- xgboost(data=train_hashed,label = X_train$amount,
                      nfold = 5, maximize = FALSE, 
                      nrounds = 100, params=param1)
y_pred <- predict(cv_xgboost, test_hashed)


submission<-data.table(X_test[,c("customer_id","mcc_code"),with=F],volume=(exp(y_pred)-1))
colnames(submission)<-c("customer_id",	"mcc_code",	"volume")
write.csv(submission, 'output/task3/submission_xgb_md10.csv', row.names = F,quote=FALSE)
