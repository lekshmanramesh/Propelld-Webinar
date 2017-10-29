/*
library(readr)
insurance_data <- read.csv("D:/Lekshman/propelld/insurance data.csv",  stringsAsFactors = F)
insurance_data[,1]=NULL
insurance_data_train=insurance_data[1:5500,]
insurance_data_train=insurance_data_train[insurance_data_train$Customer.Lifetime.Value<=20000,]
insurance_data_val=insurance_data[5501:7200,]
insurance_data_test=insurance_data[7201:9134,]
*/
library(lubridate)
date_trans=NULL
basedate=as.Date("1/1/2012", format="%d/%m/%Y")
effectivedate=as.Date(insurance_data_train$Effective.To.Date,"%m/%d/%Y")
target=insurance_data_train2$Customer.Lifetime.Value
Correlation=cor(target,as.numeric(basedate-effectivedate))
a=data.frame(Transformation="Days From 2012", Correlation=Correlation)
date_trans=rbind(date_trans,a)
Correlation=cor(target,as.numeric(month(effectivedate)))
a=data.frame(Transformation="Month", Correlation=Correlation)
date_trans=rbind(date_trans,a)
Correlation=cor(target,as.numeric((basedate-effectivedate))%%7)
a=data.frame(Transformation="Weekday", Correlation=Correlation)
date_trans=rbind(date_trans,a)