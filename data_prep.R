#### DATA LOAD ####

if(file.exists("output/transactions.csv")==F){
  transactions<-fread("data/transactions.csv")
  holidays_dt<-fread("data/holidays_list.csv")
  holidays_list<-as.Date(holidays_dt$holidays_list,format="%d.%m.%y")
  transactions$day<-sapply(transactions$tr_datetime,function(x) unlist(strsplit(x," "))[1])
  transactions$time<-sapply(transactions$tr_datetime,function(x) unlist(strsplit(x," "))[2])
  transactions$time<-hms(transactions$time)
  transactions$hour<-hour(transactions$time)
  transactions$time<-NULL
  gender <- fread("data/customers_gender_train.csv")
  tr_types_freq<-transactions[,.(sum(abs(amount))),by=tr_type]
  tr_types_freq$share<-tr_types_freq$V1/sum(tr_types_freq$V1)
  tr_types_freq$tr_type_new<-ifelse(tr_types_freq$share<0.001,"Other",tr_types_freq$tr_type)
  mcc_code_freq<-transactions[,.(sum(abs(amount))),by=mcc_code]
  mcc_code_freq$share<-mcc_code_freq$V1/sum(mcc_code_freq$V1)
  mcc_code_freq$mcc_code_new<-ifelse(mcc_code_freq$share<0.0005,"Other",mcc_code_freq$mcc_code)
  transactions<-merge(transactions,tr_types_freq[,c("tr_type","tr_type_new"),with=F],by="tr_type")
  transactions<-merge(transactions,mcc_code_freq[,c("mcc_code","mcc_code_new"),with=F],by="mcc_code")
  transactions$direction<-ifelse(transactions$amount<0,"outbound","inbound")
  transactions$amount_real<-transactions$amount/(pi^exp(1))
  transactions$date<-as.Date("2014/8/1")+as.numeric(transactions$day)
  transactions$year<-year(transactions$date)
  transactions$month<-month(transactions$date)
  transactions$day<-mday(transactions$date)
  transactions$weekday<-wday(transactions$date)
  transactions$is_holiday<-as.integer(transactions$date %in% holidays_list)
  transactions$is_weekend<-as.integer(transactions$weekday %in% c(7,1))
  write.csv(transactions,"output/transactions.csv")
} else {
  transactions<-fread("output/transactions.csv")
}