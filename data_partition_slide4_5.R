library(readr)
insurance_data <- read.csv("D:/Lekshman/propelld/insurance data.csv",  stringsAsFactors = F)
insurance_data[,1]=NULL
insurance_data_train=insurance_data[1:5500,]
insurance_data_train=insurance_data_train[insurance_data_train$Customer.Lifetime.Value<=20000,]
insurance_data_val=insurance_data[5501:7200,]
insurance_data_test=insurance_data[7201:9134,]
