library(readr)
insurance_data <- read.csv("D:/Lekshman/propelld/insurance data.csv",  stringsAsFactors = F)
insurance_data[,1]=NULL
insurance_data_train=insurance_data[1:5500,]
insurance_data_train=insurance_data_train[insurance_data_train$Customer.Lifetime.Value<=20000,]
insurance_data_val=insurance_data[5501:7200,]
insurance_data_test=insurance_data[7201:9134,]
class_check=data.frame(features=colnames(insurance_data_train), class=sapply(insurance_data_train,class))
class_num=class_check[class_check$class=="numeric",]
class_int=class_check[class_check$class=="integer",]
class_char=class_check[class_check$class=="character",]
class_char=class_char[(class_char$features)!="Effective.To.Date",]
class_num=class_num[class_num$features!="Customer.Lifetime.Value",]
insurance_data_train2=insurance_data_train
for (i in (1:nrow(class_char))){
  feature=as.character(class_char[i,1])
  a=cbind(freq=table(insurance_data_train[feature]),
                   mean_cv=tapply(insurance_data_train$Customer.Lifetime.Value, insurance_data_train[feature], mean))
  eval(parse(text = paste0('write.csv(a,"',feature,'.csv")')))
  a_df=data.frame(name_char=row.names(a), val=a[,2])
  feature_df=data.frame(feature=insurance_data_train[feature])
  feature_df$id=1:nrow(insurance_data_train)
  check=merge(feature_df,a_df,by.x=feature,by.y="name_char",all.x = T)
  check=check[order(check$id),]
  View(check)
  insurance_data_train2[feature]=check$val
}
