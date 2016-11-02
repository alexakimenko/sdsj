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
predictor_cols<-names(trans_w_g)[!(names(trans_w_g) %in% c("customer_id","V1","tr_type","tr_datetime","term_id","amount","tr_type_new",
                                                           "direction","amount_real","date","gender","mcc_code_new"))]
flowers<-trans_w_g[mcc_code=="5992" & (date==as.Date('2015/2/14') | date==as.Date('2015/3/8')) & direction == "outbound",.(sum(amount),.N),by=.(customer_id)]
X_train<-trans_w_g[is.na(gender)==F,]
X_test<-trans_w_g[is.na(gender)==T,]
train_hashed <- hashed.model.matrix(~., data=X_train[,predictor_cols,with=F], hash.size=2^8, transpose=FALSE)
train_hashed <- as(train_hashed, "dgCMatrix")
test_hashed <- hashed.model.matrix(~., data=X_test[,predictor_cols,with=F], hash.size=2^8, transpose=FALSE)
test_hashed <- as(test_hashed, "dgCMatrix")
# glmnetModel <- cv.glmnet(train_hashed, X_train$gender, nfolds = 5,
#                         family = "binomial",alpha=1,type.measure = "auc")
# saveRDS(glmnetModel,"output/task1/glmnetModel_trans.rds")
# y_train_pred <- predict(glmnetModel, train_hashed, s="lambda.min",type = "response")
# y_pred <- predict(glmnetModel, test_hashed, s="lambda.min",type = "response")
# auc(X_train$gender,as.vector(y_train_pred)) #0.5753113

summary<-data.table()
for (e in c(0.01,  0.1)){
  for (max_depth in c( 4, 6, 8)){
    for (min_c_w in c(30)){
      col_bytree=0.3
      param1 <- list("objective" = "binary:logistic", 
                     "eval_metric" = "auc", 
                     "max_depth" = max_depth, 
                     "eta" = e,
                     "min_child_weight" = min_c_w,
                     "colsample_bytree"=col_bytree)
      cv_xgboost <- xgb.cv(data=train_hashed,label = X_train$gender,
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
param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 8, 
               "eta" = 0.1,
               "min_child_weight" = 30,
               "colsample_bytree"=0.3)
fit_xgboost <- xgboost(data=train_hashed,label = X_train$gender,maximize = FALSE,
                       nrounds = 100, params=param1)
y_pred<-predict(fit_xgboost,train_hashed)
y_test_pred<-predict(fit_xgboost,test_hashed)
X_train<-cbind(X_train,y_pred)
X_test<-cbind(X_test,y_test_pred)
saveRDS(X_train,"output/task1/X_train_acct.rds")
saveRDS(X_test,"output/task1/X_test_acct.rds")

### model 1 - transactions level ###

X_train_acct<-readRDS("output/task1/X_train_acct.rds")
X_test_acct<-readRDS("output/task1/X_test_acct.rds")
X_train_add<-X_train_acct[,.(mean_y_pred=mean(y_pred),median_y_pred=median(y_pred),max_y_pred=max(y_pred),min_y_pred=min(y_pred),
                        sd_pred=sd(y_pred),mean_weighted=sum(y_pred*amount)/sum(amount)),by=.(customer_id,direction)]


X_train_add_m<-melt(X_train_add,id=c("customer_id","direction"))
X_train_add_m$variable<-paste(X_train_add_m$direction,X_train_add_m$variable)
X_train_add_c<-dcast(X_train_add_m,customer_id~variable)
X_train_add_c[is.na(X_train_add_c)]<-0
X_test_add<-X_test_acct[,.(mean_y_pred=mean(y_test_pred),median_y_pred=median(y_test_pred),max_y_pred=max(y_test_pred),min_y_pred=min(y_test_pred),
                        sd_pred=sd(y_test_pred),mean_weighted=sum(y_test_pred*amount)/sum(amount)),by=.(customer_id,direction)]
X_test_add_m<-melt(X_test_add,id=c("customer_id","direction"))
X_test_add_m$variable<-paste(X_test_add_m$direction,X_test_add_m$variable)
X_test_add_c<-dcast(X_test_add_m,customer_id~variable)
X_test_add_c[is.na(X_test_add_c)]<-0

X_train_add_c<-merge(X_train_add_c,gender,by="customer_id")
predictor_cols<-names(X_train_add_c)[!(names(X_train_add_c) %in% c("customer_id","gender"))]
X_train<-get_matrix(X_train_add_c[,predictor_cols,with=F])
X_test<-get_matrix(X_test_add_c[,predictor_cols,with=F])
xgbMatrix <- xgb.DMatrix(data=X_train, 
                         label = X_train_add_c$gender)
param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 2, 
               "eta" = 0.05,
               "min_child_weight" = 50,
               "colsample_bytree"=0.3)
cv_xgboost <- xgb.cv(data=xgbMatrix,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1)
fit_xgboost <- xgb.train(data=xgbMatrix,
                         maximize = FALSE, 
                         nrounds = 100, params=param1)
y_acct_train_pred<-predict(fit_xgboost,X_train)
y_acct_test_pred<-predict(fit_xgboost,X_test)
X_test_trans<-X_test_add_c
X_train_trans<-X_train_add_c


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
total<-merge(cm_mcc_c,cm_tr_types_c,by="customer_id",all.x = TRUE)
total<-merge(total,gender,by="customer_id",all.x = TRUE)
predictor_cols<-colnames(total)[!(colnames(total) %in% c("customer_id","gender"))]
X_train<-total[is.na(total$gender)==F,]
X_test<-total[is.na(total$gender)==T,]
X_train_mx<-get_matrix(X_train[,predictor_cols,with=F])
X_test_mx<-get_matrix(X_test[,predictor_cols,with=F])
X_train_mx[is.na(X_train_mx)]<-0
X_train_mx[is.na(X_train_mx)]<-0
xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
                         label = X_train$gender)

param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 3, 
               "eta" = 0.05,
               "subsample"=1.0, 
               "min_weight_fraction_leaf"=0,
               "min_samples_split"= 2)
cv_xgboost <- xgb.cv(data=xgbMatrix,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1)
fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
                       nrounds = 100, params=param1)
y_base_test_pred<-predict(fit_xgboost,X_test_mx)
X_base_train<-X_train
X_base_test<-X_test


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
total<-merge(cm_mcc_c,gender,by="customer_id",all.x = TRUE)
predictor_cols<-colnames(total)[!(colnames(total) %in% c("customer_id","gender"))]

X_train<-total[is.na(total$gender)==F,]
X_test<-total[is.na(total$gender)==T,]
X_train_mx<-get_matrix(X_train[,predictor_cols2,with=F])
X_test_mx<-get_matrix(X_test[,predictor_cols2,with=F])
X_train_mx[is.na(X_train_mx)]<-0
X_test_mx[is.na(X_test_mx)]<-0
xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
                         label = X_train$gender)

param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 3, 
               "eta" = 0.05,
               "subsample"=1.0, 
               "min_weight_fraction_leaf"=0,
               "min_samples_split"= 2)
cv_xgboost <- xgb.cv(data=xgbMatrix,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1)
fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
                       nrounds = 100, params=param1)

predictor_cols2<-xgb.importance(feature_names = colnames(X_train_mx), filename_dump = NULL, model = fit_xgboost)$Feature[1:50]
y_relative_test_pred<-predict(fit_xgboost,X_test_mx)

X_relat_train<-X_train
X_relat_test<-X_test


### merging 3 datasets ###

X_total_train<-merge(X_relat_train,X_train_trans,by="customer_id")
X_total_train<-merge(X_total_train,X_base_train,by="customer_id")
X_total_test<-merge(X_relat_test,X_test_trans,by="customer_id")
X_total_test<-merge(X_total_test,X_base_test,by="customer_id")
predictor_cols<-colnames(X_total_train)[!(colnames(X_total_train) %in% c("customer_id","gender","gender.x","gender.y"))]
X_train_mx<-get_matrix(X_total_train[,predictor_cols,with=F])
X_test_mx<-get_matrix(X_total_test[,predictor_cols,with=F])
X_train_mx[is.na(X_train_mx)]<-0
X_test_mx[is.na(X_test_mx)]<-0
xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
                         label = X_total_train$gender.x)

param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 3, 
               "eta" = 0.05,
               "subsample"=1.0, 
               "min_weight_fraction_leaf"=0,
               "min_samples_split"= 2)
cv_xgboost <- xgb.cv(data=xgbMatrix,
                     nfold = 5, maximize = FALSE, 
                     nrounds = 100, params=param1)
fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
                       nrounds = 100, params=param1)

predictor_cols2<-xgb.importance(feature_names = colnames(X_train_mx), filename_dump = NULL, model = fit_xgboost)$Feature[1:30]

X_train_mx<-get_matrix(X_total_train[,predictor_cols2,with=F])
X_test_mx<-get_matrix(X_total_test[,predictor_cols2,with=F])
X_train_mx[is.na(X_train_mx)]<-0
X_test_mx[is.na(X_test_mx)]<-0
xgbMatrix <- xgb.DMatrix(data=X_train_mx, 
                         label = X_total_train$gender)

summary<-data.table()
for (e in c(0.01, 0,05, 0.1)){
  for (max_depth in c(2, 3, 4)){
    for (min_c_w in c(30,60)){
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

