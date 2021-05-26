# this script is for post analysis of thief results
rm(list=ls())
model<-'nnet'
# setting working directory
setwd('D:/University/Paper/Quilty_precipitation/Canopex/Code_NewHorizon/')
# loading data
# loading the forecasted data from thief
# the output is thief_list
load(sprintf("%s_new_thief_reconciled_v1.RData",model))
# loading the test set for comparision
# the output is test listi
load(sprintf("%s_thief_test_all.RData",model))
# loading the original forecast
# the output is prediction listi
load(sprintf("%s_thief_prediction_all.RData",model))
source('ErrorFunctions_v1.R')
station<-names(test_listi)
station_number<-length(test_listi)
error_terms<-c("RMSE",'NRMSE',"MAE",'NMAE','R2')
error_number<-length(error_terms)
time<-names(test_listi[[1]][1:4])
time<-time[-3]
time_number<-length(time)

# creating empty matrix for error. the third dimension is each timescale

#original forecast
error.orf=array(dim=c(station_number,error_number,time_number))
colnames(error.orf)<-error_terms
rownames(error.orf)<-station

#HLS reconcile
error.hls=array(dim=c(station_number,error_number,time_number))
colnames(error.orf)<-error_terms
rownames(error.orf)<-station


#OLS reconcile
error.ols=array(dim=c(station_number,error_number,time_number))
colnames(error.orf)<-error_terms
rownames(error.orf)<-station

#BU reconcile
error.bu=array(dim=c(station_number,error_number,time_number))
colnames(error.orf)<-error_terms
rownames(error.orf)<-station

for (ii in seq_along(station)){
  for (jj in time){
    Y<-test_listi[[ii]]$jj
    error.orf[ii,,jj]<-c(RMSE(Prediction_listi[[ii]][[jj]],Y),
        NRMSE(Prediction_listi[[ii]][[jj]],Y),MAE(Prediction_listi[[ii]][[jj]],Y),
        NMAE(Prediction_listi[[ii]][[jj]],Y),R2(Prediction_listi[[ii]][[jj]],Y))
    
    error.hls[ii,,jj]<-c(RMSE(thief_list[[ii]][[1]][[jj]],Y),
        NRMSE(thief_list[[ii]][[1]][[jj]],Y),MAE(thief_list[[ii]][[1]][[jj]],Y),
        NMAE(thief_list[[ii]][[1]][[jj]],Y),R2(thief_list[[ii]][[1]][[jj]],Y))
    
    
    error.ols[ii,,jj]<-c(RMSE(thief_list[[ii]][[3]][[jj]],Y),
          NRMSE(thief_list[[ii]][[3]][[jj]],Y),MAE(thief_list[[ii]][[3]][[jj]],Y),
          NMAE(thief_list[[ii]][[3]][[jj]],Y),R2(thief_list[[ii]][[3]][[jj]],Y))
    
    error.bu[ii,,jj]<-c(RMSE(thief_list[[ii]][[4]][[jj]],Y),
          NRMSE(thief_list[[ii]][[4]][[jj]],Y),MAE(thief_list[[ii]][[4]][[jj]],Y),
          NMAE(thief_list[[ii]][[4]][[jj]],Y),R2(thief_list[[ii]][[4]][[jj]],Y))
    
  }
}
# average error matrix
error.orf.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.hls.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.ols.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.bu.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
# calculating the average error
for (ii in seq_along(time)){
  error.orf.mean[ii,]<-apply(error.orf[,,ii],2,mean)
  error.hls.mean[ii,]<-apply(error.hls[,,ii],2,mean)
  error.wls.mean[ii,]<-apply(error.wls[,,ii],2,mean)
  error.ols.mean[ii,]<-apply(error.ols[,,ii],2,mean)
  error.bu.mean[ii,]<-apply(error.bu[,,ii],2,mean)
}
# calculating the relative error compared to ORF
error.hls.rel<-(error.hls-error.orf)/(error.orf)*100
error.wls.rel<-(error.wls-error.orf)/(error.orf)*100
error.ols.rel<-(error.ols-error.orf)/(error.orf)*100
error.bu.rel<-(error.bu-error.orf)/(error.orf)*100

# average error matrix
error.hls.rel.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.wls.rel.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.ols.rel.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))
error.bu.rel.mean=matrix(nrow =time_number,ncol=error_number,dimnames =list(time,error_terms))

for (ii in seq_along(time)){
  error.hls.rel.mean[ii,]<-apply(error.hls.rel[,,ii],2,mean)
  error.wls.rel.mean[ii,]<-apply(error.wls.rel[,,ii],2,mean)
  error.ols.rel.mean[ii,]<-apply(error.ols.rel[,,ii],2,mean)
  error.bu.rel.mean[ii,]<-apply(error.bu.rel[,,ii],2,mean)
}
# sorting by nrmse
error.hls.rel.sorted<-error.hls.rel[order(error.orf[,2,1]),,]
mean(error.hls.rel.sorted[1:70,2,1])
