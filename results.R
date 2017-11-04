model1=lm(Customer.Lifetime.Value~.,data=insurance_data_train2)
summary(model1)

##Character Variables of Val Data

insurance_data_val2=insurance_data_val
for (i in (1:nrow(class_char))){
  feature=as.character(class_char[i,1])
  a=cbind(freq=table(insurance_data_train[feature]),
          mean_cv=tapply(insurance_data_train$Customer.Lifetime.Value, insurance_data_train[feature], mean))
  #eval(parse(text = paste0('write.csv(a,"',feature,'.csv")')))
  a_df=data.frame(name_char=row.names(a), val=a[,2])
  feature_df=data.frame(feature=insurance_data_val[feature])
  feature_df$id=1:nrow(insurance_data_val)
  check=merge(feature_df,a_df,by.x=feature,by.y="name_char",all.x = T)
  check=check[order(check$id),]
  insurance_data_val2[feature]=check$val
}

## Keeping Select Features
insurance_data_val2$log_no.policies=log(insurance_data_val2$Number.of.Policies)
insurance_data_val2$Number.of.Policies=NULL
insurance_data_val2$poly_Number.of.Open.Complaints=(insurance_data_val2$Number.of.Open.Complaints)^2
insurance_data_val2$Number.of.Open.Complaints=NULL
insurance_data_val2=subset(insurance_data_val2, select=final_vars)
insurance_data_val2$Customer.Lifetime.Value=insurance_data_val$Customer.Lifetime.Value
insurance_data_val2$pred_cltv=predict(model1,insurance_data_val2)
insurance_data_val2$per_diff=abs((insurance_data_val2$Customer.Lifetime.Value-
                                      insurance_data_val2$pred_cltv)/insurance_data_val2$Customer.Lifetime.Value)
mean(insurance_data_val2$per_diff)
insurance_data_val2$cltv_band=cut(insurance_data_val2$Customer.Lifetime.Value, breaks=c(0,3000,6000,9000,100000))
tapply(insurance_data_val2$per_diff, insurance_data_val2$cltv_band, mean)

## Replicating for Test Data

insurance_data_test2=insurance_data_test
for (i in (1:nrow(class_char))){
  feature=as.character(class_char[i,1])
  a=cbind(freq=table(insurance_data_train[feature]),
          mean_cv=tapply(insurance_data_train$Customer.Lifetime.Value, insurance_data_train[feature], mean))
  #eval(parse(text = paste0('write.csv(a,"',feature,'.csv")')))
  a_df=data.frame(name_char=row.names(a), val=a[,2])
  feature_df=data.frame(feature=insurance_data_test[feature])
  feature_df$id=1:nrow(insurance_data_test)
  check=merge(feature_df,a_df,by.x=feature,by.y="name_char",all.x = T)
  check=check[order(check$id),]
  insurance_data_test2[feature]=check$val
}

## Keeping Select Features
insurance_data_test2$log_no.policies=log(insurance_data_test2$Number.of.Policies)
insurance_data_test2$Number.of.Policies=NULL
insurance_data_test2$poly_Number.of.Open.Complaints=(insurance_data_test2$Number.of.Open.Complaints)^2
insurance_data_test2$Number.of.Open.Complaints=NULL
insurance_data_test2=subset(insurance_data_test2, select=final_vars)
insurance_data_test2$Customer.Lifetime.Value=insurance_data_test$Customer.Lifetime.Value
insurance_data_test2$pred_cltv=predict(model1,insurance_data_test2)
insurance_data_test2$per_diff=abs((insurance_data_test2$Customer.Lifetime.Value-
                                     insurance_data_test2$pred_cltv)/insurance_data_test2$Customer.Lifetime.Value)
mean(insurance_data_test2$per_diff)
insurance_data_test2$cltv_band=cut(insurance_data_test2$Customer.Lifetime.Value, breaks=c(0,3000,6000,9000,100000))
tapply(insurance_data_test2$per_diff, insurance_data_test2$cltv_band, mean)
table(insurance_data_test2$cltv_band)