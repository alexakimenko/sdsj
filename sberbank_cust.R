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
# trans_w_g<-fread("output/task1/trans_w_g.csv")
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
transactions$date<-as.Date(transactions$date,format="%Y-%m-%d")
transactions$year_month<-format(transactions$date,"%Y-%m-%01")
cm_account_margin<-transactions[,.(inbound_amnt=log(sum(amount[which( amount > 0)])+1),outbound_amnt=log(-sum(amount[which( amount < 0)])+1)),by=.(year_month,customer_id)]
cm_account_margin$year_month<-as.Date(cm_account_margin$year_month,format="%Y-%m-%d")
for (month_shift in c(1,2,3,12)){
  cm_account_margin_shift<-cm_account_margin
  cm_account_margin_shift$year_month<-cm_account_margin_shift$year_month+months(month_shift)
  cm_account_margin<-merge(cm_account_margin,cm_account_margin_shift[,c("year_month", "customer_id","inbound_amnt","outbound_amnt"),with=F],by=c("year_month","customer_id"),all=TRUE)
  names(cm_account_margin)[names(cm_account_margin)=="inbound_amnt.x"] <- "inbound_amnt"
  names(cm_account_margin)[names(cm_account_margin)=="inbound_amnt.y"] <- paste0("inbound_amnt_lag_",month_shift,"m")
  names(cm_account_margin)[names(cm_account_margin)=="outbound_amnt.x"] <- "outbound_amnt"
  names(cm_account_margin)[names(cm_account_margin)=="outbound_amnt.y"] <- paste0("outbound_amnt_lag_",month_shift,"m")
}
cm_account_margin[is.na(cm_account_margin)]<-0
cm_account_margin$inbound_amnt<-NULL
cm_account_margin$outbound_amnt<-NULL
cm_account_margin$inbound_amnt_l3m<-cm_account_margin$inbound_amnt_lag_1m+cm_account_margin$inbound_amnt_lag_2m+cm_account_margin$inbound_amnt_lag_3m
cm_account_margin$outbound_amnt_l3m<-cm_account_margin$outbound_amnt_lag_1m+cm_account_margin$outbound_amnt_lag_2m+cm_account_margin$outbound_amnt_lag_3m
cm_account_margin$inbound_amnt_l2m<-cm_account_margin$inbound_amnt_lag_1m+cm_account_margin$inbound_amnt_lag_2m+cm_account_margin$inbound_amnt_lag_2m
cm_account_margin$outbound_amnt_l2m<-cm_account_margin$outbound_amnt_lag_1m+cm_account_margin$outbound_amnt_lag_2m+cm_account_margin$outbound_amnt_lag_2m
cm_account_margin$pay_ratio_1_3<-cm_account_margin$outbound_amnt_lag_1m/(cm_account_margin$inbound_amnt_l3m+1)
cm_account_margin$pay_ratio_1_2<-cm_account_margin$outbound_amnt_lag_1m/(cm_account_margin$inbound_amnt_l2m+1)

dates<-c(unique(mcc_by_days$date),seq.Date(max(mcc_by_days$date)+1,by="day",length.out = 30))
X<-data.table(date=dates,is_holiday=as.integer(dates %in% holidays_list))
X$year_month<-format(X$date,"%Y-%m-%01")
X$year<-year(X$date)
X$month<-month(X$date)
X$day<-day(X$date)
X$week<-week(X$date)
X$weekday<-as.factor(wday(X$date))
X$weekend<-as.integer(X$weekday %in% c(7,1))
calendar_stats<-X[,.(days=max(day),holidays=sum(is_holiday),work_days=(max(day)-sum(is_holiday)-sum(weekend))),
                  by=.(year,month,year_month)]
calendar_stats<-calendar_stats[order(year_month),]
calendar_stats$weights<-c(0,1,2,3,3,3,3,3,3,3,3,3,4,4,4,4)
grid<-as.data.table(expand.grid(year_month=calendar_stats$year_month,mcc_code=all_mcc,customer_id=all_cust))
total<-merge(grid,mcc_by_days[,.(amount=sum(amount)),by=.(year_month,mcc_code,customer_id)],by=c("year_month","mcc_code","customer_id"),all.x=TRUE)
total<-merge(total,calendar_stats,by="year_month")
total$year_month<-as.Date(total$year_month)
total[is.na(total)]<-0
total$amount<-log(-total$amount+1)
for (month_shift in c(1,2,3,12)){
  total_shift<-total
  total_shift$year_month<-total_shift$year_month+months(month_shift)
  total<-merge(total,total_shift[,c("year_month","mcc_code","customer_id","amount"),with=F],by=c("year_month","mcc_code","customer_id"),all.x=TRUE)
  names(total)[names(total)=="amount.x"] <- "amount"
  names(total)[names(total)=="amount.y"] <- paste0("amount_lag_",month_shift,"m")
}

total<-merge(total,cm_account_margin,by=c("year_month","customer_id"),all.x=T)
total[is.na(total)]<-0
total$amount_l2m_avg<-(total$amount_lag_1m+total$amount_lag_2m)/2
total$amount_l3m_avg<-(total$amount_lag_1m+total$amount_lag_2m+total$amount_lag_3m)/3
total$customer_id<-as.factor(total$customer_id)
total$mcc_code<-as.factor(total$mcc_code)
total$amount_bin<-total$amount>0
total$mcc_pay_ratio_1_3<-total$amount_lag_1m/(total$inbound_amnt_l3m+1)
total$mcc_pay_ratio_1_2<-total$amount_lag_1m/(total$inbound_amnt_l2m+1)
total$mcc_pay_ratio_1_1<-total$amount_lag_1m/(total$inbound_amnt_lag_1m+1)
predictor_cols <- names(total)[!(names(total) %in% c("year_month","amount","amount_bin","weights"))]
mx_cols <- names(total)[!(names(total) %in% c("year_month","amount","amount_bin","weights","customer_id","mcc_code"))]
X_train<-total[!(as.character(year_month) %in% c("2015-11-01","2015-01-01","2014-08-01")),]
X_test<-total[year_month=="2015-11-01",]
train_hashed <- hashed.model.matrix(~., data=X_train[,predictor_cols,with=F], hash.size=2^9, transpose=FALSE)
train_hashed <- as(train_hashed, "dgCMatrix")
test_hashed <- hashed.model.matrix(~., data=X_test[,predictor_cols,with=F], hash.size=2^9, transpose=FALSE)
test_hashed <- as(test_hashed, "dgCMatrix")
fit_glmnet <- cv.glmnet(train_hashed, X_train$amount_bin, nfolds = 5,
                         family = "binomial",alpha=1,type.measure = "auc",weights=X_train$weights)
X_train$glmnet_pred <- predict(fit_glmnet, train_hashed, s="lambda.min")
X_test$glmnet_pred <- predict(fit_glmnet, test_hashed, s="lambda.min")
train_hashed <- hashed.model.matrix(~., data=X_train[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
train_hashed <- as(train_hashed, "dgCMatrix")
test_hashed <- hashed.model.matrix(~., data=X_test[,predictor_cols,with=F], hash.size=2^13, transpose=FALSE)
test_hashed <- as(test_hashed, "dgCMatrix")
xgb_train_hashed <- xgb.DMatrix(train_hashed, 
                         label = X_train$amount, 
                         weight = X_train$weights)
summary<-data.table()
for (e in c(0.01,  0.1)){ ### запустить отсюда!
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
cv_xgboost <- xgboost(data=xgb_train_hashed,label = X_train$amount,
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
cv_xgboost <- xgb.cv(data=as.matrix(X_train[,mx_cols,with=F]),label = X_train$amount,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1) 
fit_xgboost <- xgboost(data=as.matrix(X_train[,mx_cols,with=F]),label = X_train$amount,
                      nfold = 5, maximize = FALSE, 
                      nrounds = 100, params=param1)
y_pred <- pmax(predict(cv_xgboost, test_hashed),0)
# test-rmse:1.261397+0.003217 weights 


train_hashed_cm <- hashed.model.matrix(~., data=X_train[,2,with=F], hash.size=2^13, transpose=FALSE)
train_mx_cm<-as.matrix(train_hashed_cm)
train_hashed_cm_2<-cbind(train_hashed_cm,X_train[,4,with=F])
train_hashed_cm <- hashed.model.matrix(~., data=X_train[,4,with=F], hash.size=2^13, transpose=FALSE)
predictor_cols2<-xgb.importance(feature_names = mx_cols, model = fit_xgboost)$Feature[1:300]

submission<-data.table(X_test[,c("customer_id","mcc_code"),with=F],volume=(exp(y_pred)-1))
colnames(submission)<-c("customer_id",	"mcc_code",	"volume")
write.csv(submission, 'output/task3/submission_xgb_md10_rmse_1.22.csv', row.names = F,quote=FALSE)

hist(transactions$amount/(pi^exp(1))[transactions$year_month=="2015-10-01"])
hist(submission$volume/(pi^exp(1))[submission$volume/(pi^exp(1))<100])

t<-submission[volume<100*pi^exp(1),"volume",with=F]
t2<-transactions[amount>-100*pi^exp(1)&direction=="outbound","amount",with=F]
table(t2/100*pi^exp(1))
submission$volume<-pmax(submission$volume,0)
