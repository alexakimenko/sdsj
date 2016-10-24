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


source("data_prep.R")
gender<-fread("data/customers_gender_train.csv")
holidays_dt<-fread("data/holidays_list.csv")
holidays_list<-as.Date(holidays_dt$holidays_list,format="%d.%m.%y")


#### DATASET 1 ####
cm_base_for_mcc<-transactions[,.(.N,sum_amnt=sum(amount)),
                      by=.(customer_id,direction)]
cm_base<-transactions[,.(weekday=mean(weekday)),
                        by=.(customer_id)]
cm_tr_types<-transactions[,.(.N),
                      by=.(customer_id,tr_type_new)]

cm_tr_types_c<-dcast(cm_tr_types,customer_id~tr_type_new)
cm_tr_types_c[is.na(cm_tr_types_c)]<-0
cm_tr_types_c<-merge(cm_tr_types_c,cm_base,by="customer_id")
cm_mcc<-transactions[,.(.N,sum_amnt=sum(amount)),
                            by=.(customer_id,mcc_code,direction)]
cm_mcc<-merge(cm_mcc,cm_base_for_mcc,by=c("customer_id","direction"))
cm_mcc$share_n<-cm_mcc$N.x/cm_mcc$N.y
cm_mcc$share_amnt<-cm_mcc$sum_amnt.x/cm_mcc$sum_amnt.y
cm_mcc$N.y<-NULL
cm_mcc$sum_amnt.y<-NULL
cm_mcc$sum_amnt.x<-NULL
cm_mcc_m<-melt(cm_mcc,id=c("customer_id","mcc_code","direction"))
cm_mcc_m$var_name<-paste(cm_mcc_m$mcc_code,cm_mcc_m$direction,cm_mcc_m$variable,sep="_")
cm_mcc_c<-dcast(cm_mcc_m,customer_id~var_name)
cm_mcc_c[is.na(cm_mcc_c)]<-0
total<-merge(cm_mcc_c,cm_tr_types_c,by="customer_id",all.x = TRUE)
total<-merge(total,gender,by="customer_id",all.x = TRUE)
train<-total[is.na(total$gender)==F,]
test<-total[is.na(total$gender)==T,]
y<-train$gender
train$gender<-NULL
test$gender<-NULL
train$customer_id<-NULL
test$customer_id<-NULL
saveRDS(train,"output/task1/train.rds")
saveRDS(test,"output/task1/test.rds")
saveRDS(y,"output/task1/y.rds")


#### DATASET 2 ####

cm_base<-transactions[,.(weekday=mean(weekday)),
                      by=.(customer_id)]
cm_tr_types<-transactions[,.(.N),
                          by=.(customer_id,tr_type_new)]
cm_tr_types_c<-dcast(cm_tr_types,customer_id~tr_type_new)
cm_tr_types_c[is.na(cm_tr_types_c)]<-0
cm_tr_types_c<-merge(cm_tr_types_c,cm_base,by="customer_id")
cm_mcc<-transactions[,.(.N),
                     by=.(customer_id,mcc_code_new)]
cm_mcc$variable<-paste0("mcc_code_",cm_mcc$mcc_code_new)
cm_mcc_c<-dcast(cm_mcc,customer_id~variable,value.var = "N")
cm_mcc_c[is.na(cm_mcc_c)]<-0
total<-merge(cm_mcc_c,cm_tr_types_c,by="customer_id",all.x = TRUE)
total<-merge(total,gender,by="customer_id",all.x = TRUE)
train<-total[is.na(total$gender)==F,]
test<-total[is.na(total$gender)==T,]
y<-train$gender
train$gender<-NULL
test$gender<-NULL
train$customer_id<-NULL
test$customer_id<-NULL
saveRDS(train,"output/task1/train2.rds")
saveRDS(test,"output/task1/test2.rds")
saveRDS(y,"output/task1/y.rds")

#### SAMPLING ####


train<-readRDS("output/task1/train.rds")
y<-("output/task1/y.rds")
is_train<-sample(1:nrow(train), nrow(train)*0.7,replace=FALSE)
X_train<-train[is_train,]
X_test<-train[-is_train,]
y_train<-y[is_train]==1
y_test<-y[-is_train]==1


#### CHALLENGER MODELS ####

#### GLMNET ####

X_train_mx<-get_matrix(X_train)
X_test_mx<-get_matrix(X_test)
summary<-data.table()
for (s in seq(0,1,by=0.05)){
  for (a in seq(0,1,by=0.1)){
    fit_glmnet <- glmnet(X_train_mx, y_train,alpha = a,family = "binomial")
    y_train_pred<-predict(fit_glmnet,X_train_mx,s=s,type = "response")
    y_test_pred<-predict(fit_glmnet,X_test_mx,s=s,type = "response")
    out_i<-data.frame(model=paste0("penalized regresson (alpha=",a,"lambda=",s,")"),
                                   auc_train=auc(y_train, y_train_pred),
                                   auc_test=auc(y_test, y_test_pred))
    summary<-rbind(summary,out_i)
  }
}
head(summary[order(-summary$auc_test),],100)



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

X_train<-get_matrix(train)
X_test<-get_matrix(test)
y_train<-y==1
param1 <- list("objective" = "binary:logistic", 
               "eval_metric" = "auc", 
               "max_depth" = 6, 
               "eta" = 0.1,
               "min_child_weight" = 40,
               "colsample_bytree"=0.3)
xgbMatrix <- xgb.DMatrix(data=X_train, 
                         label = y_train)
fit_xgboost <- xgboost(data=xgbMatrix,maximize = FALSE,
                       nrounds = 100, params=param1)
saveRDS(fit_xgboost,"output/task1/fit_xgboost.rds")
y_pred<-predict(fit_xgboost,X_test)
submission<-cbind(total[is.na(total$gender)==T,"customer_id",with=F],y_pred)
colnames(submission)<-c("customer_id","gender")
write.csv(submission, 'output/task1/submission.csv', row.names = F)


