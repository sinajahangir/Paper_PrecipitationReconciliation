#In this file we will not use quarterly, semi-annual, and annual
#WLS is also not considered
rm(list=ls())
# loading thief package
library("thief")
setwd('D:/University/Paper/Quilty_precipitation/Canopex')

model_name='arima'
# loading the forecasted data
# the output is prediction_listi
load(sprintf("%s_thief_prediction_all.RData",model_name))
#loading the saved model
# the output is model_listi
load(sprintf("%s_thief_model_all.RData",model_name))
#combination method
comb_method=c("struc" ,"ols", "bu")
# Craeting thief output list



station_names<-names(Prediction_listi)
thief_list<-vector("list", length(station_names))
names(thief_list) <-station_names




comb_list=vector("list",length(comb_method))
names(comb_list)<-comb_method


# adding sublist based on method
for (ii in seq_along(thief_list)){
  thief_list[[ii]]<-comb_list
}


# creating mse list
time_names<-names(Prediction_listi[[1]])
time_names<-time_names[1:4]
time_names<-time_names[-3]
mse_list<-vector("list",length(station_names))
names(mse_list)<-station_names

# obtaining mse list
for (ii in seq_along(station_names)){
  temp<-vector(length=length(time_names))
  for (jj in seq_along(time_names)){
    temp[jj]<-var(model_listi[[ii]][[jj]]$residuals,na.rm =T)
  }
  mse_list[[ii]]<-temp
  
}

for (ii in seq_along(thief_list)){
  temp<-Prediction_listi[[ii]][1:4]
  temp<-temp[-3]
  aa<-ts(temp[["Monthly"]],frequency =4)
  bb<-ts(temp[["2-Monthly"]],frequency =2)
  cc<-ts(temp[["4-Monthly"]],frequency =1)
  
  for (jj in seq_along(comb_method)){
    thief_list[[ii]][[jj]]<-reconcilethief(list(aa,bb,cc),comb=comb_method[jj])
    names(thief_list[[ii]][[jj]])<-c('Monthly','2-Monthly','4-Monthly')
  }
}
  
save(thief_list,file=sprintf("%s_new_thief_reconciled_v1.RData",model_name))
