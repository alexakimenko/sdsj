#### REQUIRED PACKAGES ####

list.of.packages <- c( "data.table" , "glmnet","randomForest", "xgboost","pROC","AUC","ROCR")
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
library(scales)
library(FeatureHashing)
library(AUC)
library(ROCR)

#### PARAMETERS ####

options(scipen=999)


####### FUNCTIONS ###########

get_matrix<-function(data_frame){
  data_frame<-as.data.frame(data_frame)
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
auc <- function(outcome, proba){
  N = length(proba)
  N_pos = sum(outcome)
  df = data.frame(out = outcome, prob = proba)
  colnames(df)<-c("out","prob")
  df = df[order(-df$prob),]
  df$above = (1:N) - cumsum(df$out)
  return( 1- sum( as.numeric(df$above * df$out ) )/  (N_pos * as.numeric(N-N_pos) ) )
}

###### DATA #####

transactions<-fread("output/transactions.csv")
gender<-fread("data/customers_gender_train.csv")




#### vis ####

test<-transactions[direction=="outbound",.(amnt=sum(amount)),by=.(date,customer_id)]
test$date<-as.Date(test$date)
test_g<-merge(test,gender,by="customer_id",all.x = T)
test_f<-test_g[,.(amnt=sum(amnt),cust=.N),by=.(date,gender)]
test_f<-test_f[order(gender),]
ggplot(test_f[month(date) %in% c(2,3),],aes(x=date,y=-amnt,fill=gender))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_date(labels = date_format("%Y-%m-%d"))


### SAMPLING ####

trans_w_g<-merge(transactions,gender,by="customer_id",all.x=T)
trans_w_g$mcc_code<-as.factor(trans_w_g$mcc_code)
trans_w_g$tr_type_new<-as.factor(trans_w_g$tr_type_new)
trans_w_g$direction<-as.factor(trans_w_g$direction)
trans_w_g$month<-as.factor(trans_w_g$month)
trans_w_g$is_holiday<-as.factor(trans_w_g$is_holiday)
trans_w_g$is_weekend<-as.factor(trans_w_g$is_weekend)
trans_w_g$hour_0<-ifelse(trans_w_g$hour==0,1,0)
trans_w_g$direction_o<-ifelse(trans_w_g$direction=="outbound",1,0)
trans_w_g$tr_type_parsed<-ifelse(trans_w_g$amount > 0 & trans_w_g$tr_type %in% c(7000),"взнос в отделении",
                                 ifelse(trans_w_g$amount > 0 & trans_w_g$tr_type %in% c(7010, 7011, 7014, 7015, 7020, 7021, 7024, 7025, 7060),"взнос ATM",
                                        ifelse(trans_w_g$amount > 0 & trans_w_g$tr_type %in% c(7050, 7081, 7082, 7084),"пополнение со счёта",
                                               ifelse(trans_w_g$amount > 0 & trans_w_g$tr_type %in% c(7030, 7031, 7034, 7035, 7040, 7041, 7044, 7045, 7070, 7071, 7074, 7075),"пополнение с карты",
                                                      ifelse(trans_w_g$amount < 0 & trans_w_g$tr_type %in% c(2000, 2001, 2010, 2011, 2020, 2021, 2100, 2110, 2200, 2210, 2900),"выдача наличных",
                                                             ifelse(trans_w_g$amount < 0 & trans_w_g$tr_type %in% c(2401, 2402, 2412, 2422, 2432),"перевод на счёт",
                                                                    ifelse(trans_w_g$amount < 0 & trans_w_g$tr_type %in% c(2330, 2331, 2340, 2341, 2370, 2371, 2440, 2446, 2456, 2460),"перевод на карту",
                                                                           ifelse(trans_w_g$amount < 0 & trans_w_g$tr_type %in% c(2320, 2323, 2325) & trans_w_g$mcc_code != 4900,"перевод юр.лицу",
                                                                                  ifelse(trans_w_g$amount < 0 & (
                                                                                    trans_w_g$tr_type %in% c(1000, 1010, 1030, 1100, 1110, 1200, 1210, 1310, 1410, 1510, 2901, 2992, 8035, 8100, 8145, 2992, 8035, 8100, 8146) | trans_w_g$mcc_code == 4900) & trans_w_g$mcc_code != -1,"покупка","other" ) ) ) ) ) )) ))


trans_w_g$amount_norm<-(trans_w_g$amount-mean(trans_w_g$amount))/sd(trans_w_g$amount)
#write.csv(trans_w_g,"output/task1/trans_w_g.csv")
trans_w_g<-fread("output/task1/trans_w_g.csv")
flowers<-trans_w_g[mcc_code=="5992" & (date==as.Date('2015/2/14') | date==as.Date('2015/3/8')) & direction == "outbound",.(flowers_amnt=sum(amount),flowers_num=.N),by=.(customer_id)]

X_train_<-trans_w_g[is.na(gender)==F,]
X_test<-trans_w_g[is.na(gender)==T,]
sample_flag<-sample(1:nrow(X_train_),nrow(X_train_)*0.7)
X_train<-X_train_[sample_flag,]
X_holdout<-X_train_[-sample_flag,]
global_mean<-mean(X_train_$gender)
for (C in c(10,5)){
  mcc_codes_y<-X_train[,.(regul_mean=(mean(gender)*.N+global_mean*C)/(.N+C),mean=mean(gender)),by=.(mcc_code)]
  mcc_codes_holdout<-X_holdout[,.(mean=mean(gender)),by=.(mcc_code)]
  test<-merge(mcc_codes_y,mcc_codes_holdout,by="mcc_code")
  print(C)
  print(mean(abs(test$regul_mean-test$mean.y)))
  print(mean(abs(test$mean.x-test$mean.y)))
}
C=10
mcc_codes_y<-X_train[,.(regul_mean=(mean(gender)*.N+global_mean*C)/(.N+C),mean=mean(gender)),by=.(mcc_code)]
cm_mcc_codes_y<-merge(trans_w_g[,.(.N),by=.(customer_id,mcc_code)],mcc_codes_y,by="mcc_code")
cm_mcc_codes_y$weight<-cm_mcc_codes_y$regul_mean*cm_mcc_codes_y$N
cm_mcc<-cm_mcc_codes_y[,.(mcc_code_weight_y=sum(weight)/sum(N),mcc_code_mean_y=mean(regul_mean),mcc_code_max_y=max(regul_mean),mcc_code_min_y=min(regul_mean)),by=.(customer_id)]

for (C in c(10,5,20,40,60,100)){
  tr_types_y<-X_train[,.(regul_mean=(mean(gender)*.N+global_mean*C)/(.N+C),mean=mean(gender)),by=.(tr_type)]
  tr_types_holdout<-X_holdout[,.(mean=mean(gender)),by=.(tr_type)]
  test<-merge(tr_types_y,tr_types_holdout,by="tr_type")
  print(C)
  print(mean(abs(test$regul_mean-test$mean.y)))
  print(mean(abs(test$mean.x-test$mean.y)))
}
C=10
tr_types_y<-X_train[,.(regul_mean=(mean(gender)*.N+global_mean*C)/(.N+C),mean=mean(gender)),by=.(tr_type)]
cm_tr_types<-merge(trans_w_g[,.(.N),by=.(customer_id,tr_type)],tr_types_y,by="tr_type")
cm_tr_types$weight<-cm_tr_types$regul_mean*cm_tr_types$N
cm_tr<-cm_tr_types[,.(tr_code_weight_y=sum(weight)/sum(N),tr_code_mean_y=mean(regul_mean),tr_code_max_y=max(regul_mean),tr_code_min_y=min(regul_mean)),by=.(customer_id)]
X_trans<-merge(cm_mcc,cm_tr,by="customer_id",all=T)
X_trans<-merge(X_trans,flowers,by="customer_id",all=T)
X_trans[is.na(X_trans)]<-0

### model 2 - base ###

cm_base<-trans_w_g[,.(weekday=mean(weekday)),
                   by=.(customer_id)]
cm_tr_types<-trans_w_g[,.(.N),
                       by=.(customer_id,tr_type_parsed)]
cm_tr_types_c<-dcast(cm_tr_types,customer_id~tr_type_parsed,value.var="N")
cm_tr_types_c[is.na(cm_tr_types_c)]<-0
cm_tr_types_c<-merge(cm_tr_types_c,cm_base,by="customer_id")
cm_mcc<-trans_w_g[,.(.N),
                  by=.(customer_id,mcc_code)]
cm_mcc$variable<-paste0("mcc_code_",cm_mcc$mcc_code)
cm_mcc_c<-dcast(cm_mcc,customer_id~variable,value.var = "N")
cm_mcc_c[is.na(cm_mcc_c)]<-0
X_base<-merge(cm_mcc_c,cm_tr_types_c,by="customer_id",all.x = TRUE)
X_base<-merge(X_base,gender,by="customer_id",all.x = TRUE)
# predictor_cols<-colnames(total)[!(colnames(total) %in% c("customer_id","gender"))]
# X_train<-total[is.na(total$gender)==F,]
# X_test<-total[is.na(total$gender)==T,]
# X_train_mx<-get_matrix(X_train[,predictor_cols,with=F])
# X_test_mx<-get_matrix(X_test[,predictor_cols,with=F])
# X_train_mx[is.na(X_train_mx)]<-0
# X_train_mx[is.na(X_train_mx)]<-0
# xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
#                          label = X_train$gender)
# 
# param1 <- list("objective" = "binary:logistic", 
#                "eval_metric" = "auc", 
#                "max_depth" = 3, 
#                "eta" = 0.05,
#                "subsample"=1.0, 
#                "min_weight_fraction_leaf"=0,
#                "min_samples_split"= 2)
# cv_xgboost <- xgb.cv(data=xgbMatrix,
#                      nfold = 5, maximize = FALSE, 
#                      nrounds = 100, params=param1)
# fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
#                        nrounds = 100, params=param1)
# y_base_test_pred<-predict(fit_xgboost,X_test_mx)
# X_base_train<-X_train
# X_base_test<-X_test


### model 3 - relative numbers ###

cm_base_for_mcc<-trans_w_g[,.(.N,sum_amnt=sum(amount)),
                           by=.(customer_id,direction)]
cm_mcc<-trans_w_g[,.(.N,sum_amnt=sum(amount)),
                  by=.(customer_id,mcc_code,direction)]
cm_mcc<-merge(cm_mcc,cm_base_for_mcc,by=c("customer_id","direction"))
cm_mcc$share_n<-cm_mcc$N.x/cm_mcc$N.y
cm_mcc$share_amnt<-cm_mcc$sum_amnt.x/cm_mcc$sum_amnt.y
cm_mcc$N.y<-NULL
cm_mcc$N.x<-NULL
cm_mcc$sum_amnt.y<-NULL
cm_mcc$sum_amnt.x<-NULL
cm_mcc_m<-melt(cm_mcc,id=c("customer_id","mcc_code","direction"))
cm_mcc_m$var_name<-paste(cm_mcc_m$mcc_code,cm_mcc_m$direction,cm_mcc_m$variable,sep="_")
cm_mcc_c<-dcast(cm_mcc_m,customer_id~var_name)
cm_mcc_c[is.na(cm_mcc_c)]<-0
X_relat<-cm_mcc_c
# X_relat<-merge(cm_mcc_c,gender,by="customer_id",all.x = TRUE)
# predictor_cols<-colnames(total)[!(colnames(total) %in% c("customer_id","gender"))]
# 
# X_train<-total[is.na(total$gender)==F,]
# X_test<-total[is.na(total$gender)==T,]
# X_train_mx<-get_matrix(X_train[,predictor_cols2,with=F])
# X_test_mx<-get_matrix(X_test[,predictor_cols2,with=F])
# X_train_mx[is.na(X_train_mx)]<-0
# X_test_mx[is.na(X_test_mx)]<-0
# xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
#                          label = X_train$gender)
# 
# param1 <- list("objective" = "binary:logistic", 
#                "eval_metric" = "auc", 
#                "max_depth" = 3, 
#                "eta" = 0.05,
#                "subsample"=1.0, 
#                "min_weight_fraction_leaf"=0,
#                "min_samples_split"= 2)
# cv_xgboost <- xgb.cv(data=xgbMatrix,
#                      nfold = 5, maximize = FALSE, 
#                      nrounds = 100, params=param1)
# fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
#                        nrounds = 100, params=param1)
# 
# predictor_cols2<-xgb.importance(feature_names = colnames(X_train_mx), filename_dump = NULL, model = fit_xgboost)$Feature[1:50]
# y_relative_test_pred<-predict(fit_xgboost,X_test_mx)
# 
# X_relat_train<-X_train
# X_relat_test<-X_test
# 

### merging 3 datasets ###

X_total<-merge(X_trans,X_base,by="customer_id")
X_total<-merge(X_total,X_relat,by="customer_id")
names(X_total)[names(X_total)=="взнос ATM"]<-"ATM"
names(X_total)[names(X_total)=="выдача наличных"]<-"cash"
names(X_total)[names(X_total)=="перевод на карту"]<-"card_transfer"
names(X_total)[names(X_total)=="покупка"]<-"purchase"
names(X_total)[names(X_total)=="пополнение с карты"]<-"inbound_from_card"
X_train_<-X_total[is.na(gender)==F]
X_test<-X_total[is.na(gender)==T]
sample_flag<-sample(1:nrow(X_train_),0.7*nrow(X_train_))
X_train<-X_train_[sample_flag,]
zero_sd<-names(X_train)[apply(X_train,2,sd)==0]
X_holdout<-X_train_[-sample_flag,]
predictor_cols<-colnames(X_total)[!(colnames(X_total) %in% c(zero_sd,"customer_id","gender","gender.x","gender.y"))]
X_train_mx<-get_matrix(X_train[,predictor_cols,with=F])
X_holdout_mx<-get_matrix(X_holdout[,predictor_cols,with=F])
X_test_mx<-get_matrix(X_test[,predictor_cols,with=F])

param1 <- list("objective" = "binary:logistic",
               "eval_metric" = "auc",
               "max_depth" = 3,
               "eta" = 0.05,
               "subsample"=1.0,
               "min_weight_fraction_leaf"=0,
               "min_samples_split"= 2,
               "col_bytree"=0.3)
fit_xgboost <- xgboost(data=X_train_mx,label=X_train$gender,maximize = FALSE,
                       nrounds = 100, params=param1)
importance_cols<-xgb.importance(feature_names = colnames(X_train_mx), filename_dump = NULL, model = fit_xgboost)$Feature
length(importance_cols)
summary_gender<-data.table()
for (ncols in c(0.5,0.8,1)){
  X_train_mx<-get_matrix(X_train[,importance_cols[1:(length(importance_cols)*ncols)],with=F])
  X_holdout_mx<-get_matrix(X_holdout[,importance_cols[1:(length(importance_cols)*ncols)],with=F])
  for (e in c(0.01, 0,05, 0.1)){
    for (max_depth in c(2, 3, 4)){
      for (min_c_w in c(30,60)){
        col_bytree=0.3
        param1 <- list("objective" = "binary:logistic", 
                       "eval_metric" = "auc", 
                       "max_depth" = max_depth, 
                       "eta" = e,
                       "min_child_weight" = min_c_w,
                       "min_weight_fraction_leaf"=0,
                       "min_samples_split"= 2,
                       "colsample_bytree"=col_bytree)
        
        cv_xgboost <- xgb.cv(data=X_train_mx,label=X_train$gender,
                             nfold = 5, maximize = FALSE, 
                             nrounds = 100, params=param1)
        fit_xgboost <- xgboost(data=X_train_mx,label=X_train$gender,maximize = FALSE,
                               nrounds = 100, params=param1)
        y_pred<-predict(fit_xgboost,X_holdout_mx)
        out_i<-data.frame(model=paste0("XGBoost ", ncol(X_train_mx),"vars (eta=",e,", max_depth=",max_depth,", colsample_bytree=",col_bytree,
                                       ", min_child_weight=",min_c_w,")"),
                          error_train=tail(cv_xgboost$train.auc.mean,1),
                          error_test=tail(cv_xgboost$test.auc.mean,1),
                          error_holdout=auc(X_holdout$gender,y_pred))
        summary_gender<-rbind(summary_gender,out_i)
      }
    }
  }  
}
summary[order(-summary$error_test),]


param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 2, 
               "eta" = 0.1,
               "min_child_weight" = 60,
               "colsample_bytree"=0.3)
fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
                       nrounds = 100, params=param1)
y_pred<-predict(fit_xgboost,X_test_mx)


submission<-data.table(customer_id=X_test$customer_id,gender=y_pred)
write.csv(submission, 'output/task1/submission_mambo_jambo.csv', row.names = F,quote = F)
