
  library(readr)
insurance_data <- read.csv("D:/Lekshman/propelld/insurance data.csv",  stringsAsFactors = F)
insurance_data[,1]=NULL
insurance_data_train=insurance_data[1:5500,]
insurance_data_train=insurance_data_train[insurance_data_train$Customer.Lifetime.Value<=20000,]
insurance_data_val=insurance_data[5501:7200,]
insurance_data_test=insurance_data[7201:9134,]
insurance_data_train2=insurance_data_train


num_trans=NULL
best_num_trans=function(feature){
  inter_df=NULL
  no_trans=insurance_data_train2[,feature]
  target=insurance_data_train2$Customer.Lifetime.Value
  corr1=cor(target,as.numeric(no_trans))
  if (min(no_trans)>0){
  inter_df=rbind(inter_df,data.frame(trans="no_trans",cor=corr1))
  trans_log=log(insurance_data_train2[,feature])
  corr2=cor(target,as.numeric(trans_log))
  inter_df=rbind(inter_df,data.frame(trans="log",cor=corr2))
  
  trans_inv=1/(insurance_data_train2[,feature])
  corr3=cor(target,as.numeric(trans_inv))
  inter_df=rbind(inter_df,data.frame(trans="inv",cor=corr3))
  }
  trans_poly=(insurance_data_train2[,feature])^2
  corr4=cor(target,as.numeric(trans_poly))
  inter_df=rbind(inter_df,data.frame(trans="poly",cor=corr4))
  trans_final=inter_df[inter_df$cor==max(inter_df$cor),]
  a=data.frame(col=feature,transformation=trans_final$trans, correlation=trans_final$cor)
  return(a)
}
for (i in (1:nrow(class_num))){
  num_trans=rbind(num_trans,best_num_trans(class_num$features[i]))
}
for (i in (1:nrow(class_int))){
  num_trans=rbind(num_trans,best_num_trans(as.character(class_int$features[i])))
}