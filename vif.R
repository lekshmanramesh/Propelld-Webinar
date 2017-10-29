insurance_data_train2=insurance_data_train2[,c("Coverage","Vehicle.Class","Renew.Offer.Type","Customer.Lifetime.Value",
                                               "Number.of.Open.Complaints","Monthly.Premium.Auto",
                                               "Marital.Status","EmploymentStatus",
                                               "Number.of.Policies")]
insurance_data_train2$log_no.policies=log(insurance_data_train2$Number.of.Policies)
insurance_data_train2$Number.of.Policies=NULL
insurance_data_train2$poly_Number.of.Open.Complaints=(insurance_data_train2$Number.of.Open.Complaints)^2
insurance_data_train2$Number.of.Open.Complaints=NULL
# x being the data frame after excluding the target variable
vif_lk=function(x){
  ans=NULL
  colnames=names(x)
  
  for (i in names(x)){
    regressors=colnames[-which(colnames==i)]
    form=as.formula(paste(i,"~",paste(regressors,collapse="+")))
    model=lm(formula=form, data=x)
    sum_model=summary(model)
    op=data.frame(col=i, rsq=sum_model$r.squared)
    ans=rbind(ans,op)
  }
  ans
}

vif_data=subset(insurance_data_train2, select=-Customer.Lifetime.Value)
vif_data=subset(vif_data, select=-Monthly.Premium.Auto)
final_vars=as.character(vif_lk(vif_data)$col)
insurance_data_train2=subset(insurance_data_train2, select=final_vars)
insurance_data_train2$Customer.Lifetime.Value=insurance_data_train$Customer.Lifetime.Value