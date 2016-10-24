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
library(scales)

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
trans_w_g<-with(trans_w_g,data.frame(trans_w_g[,!(colnames(trans_w_g) %in% c("mcc_code")),with=F],
                     model.matrix(~mcc_code-1,trans_w_g)))
trans_w_g<-with(trans_w_g,data.frame(trans_w_g[,!(colnames(trans_w_g) %in% c("month","tr_type_new"))],
                                     model.matrix(~month-1,trans_w_g),         
                                     model.matrix(~tr_type_new-1,trans_w_g)))

trans_w_g<-fread("output/task1/trans_w_g.csv")
predictor_cols<-names(trans_w_g)[!(names(trans_w_g) %in% c("customer_id","V1","tr_type","tr_datetime","term_id","amount",
                                                           "direction","amount_real","date","gender","mcc_code_new"))]
trans_w_g$amount_norm<-(trans_w_g$amount-mean(trans_w_g$amount))/sd(trans_w_g$amount)
write.csv(trans_w_g,"output/task1/trans_w_g.csv")
X_train<-trans_w_g[is.na(gender)==F,]
X_test<-trans_w_g[is.na(gender)==T,]
X_train_<-as.data.frame(X_train[,predictor_cols,with=F])
X_test_<-as.data.frame(X_test[,predictor_cols,with=F])
X_train_mx<-get_matrix(X_train_)
X_test_mx<-get_matrix(X_test_)
X_train_<-NULL
X_test_<-NULL
trans_w_g<-NULL
glmnetModel <- cv.glmnet(X_train_mx, X_train$gender, nfolds = 5,
                         family = "binomial",alpha=1,type.measure = "auc")
y_pred <- predict(glmnetModel, X_test[,predictor_cols,with=F], s="lambda.min")


