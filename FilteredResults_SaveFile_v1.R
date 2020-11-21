#This file is used for saving the average filtered Results
rm(list=ls())
# setting working directory
setwd('D:/University/Paper/Quilty_precipitation/Canopex')
filtering_scale=4
# Error Calculation -------------------------------------------------------
# the output is thief_list
load("nnet_thief_reconciled_v1.RData")
nnet_rec<-thief_list
load("ets_thief_reconciled_v1.RData")
ets_rec<-thief_list
load("arima_thief_reconciled_v1.RData")
arima_rec<-thief_list
rm(thief_list)
# loading the test set for comparision
# the output is test listi
load("nnet_thief_test_all.RData")
test_observ<-test_listi
rm(test_listi)
# loading the original forecast
# the output is prediction listi
load("nnet_thief_prediction_all.RData")
nnet_orf<-Prediction_listi
load("ets_thief_prediction_all.RData")
ets_orf<-Prediction_listi
load("arima_thief_prediction_all.RData")
arima_orf<-Prediction_listi
rm(Prediction_listi)
# load error functions
source('ErrorFunctions_v1.R')
#load error library(Metrics)
library(Metrics)
#load library(hydroGOF)
library(hydroGOF)
station<-names(test_observ)
station_number<-length(test_observ)
error_terms<-c("RMSE",'NRMSE',"MAE",'NMAE','R2','MAPE','SMAPE','MASE','Pbias','KGE','NSE')
error_number<-length(error_terms)
time<-names(test_observ[[1]])
time_number<-length(time)

# creating empty matrix for error. the third dimension is each timescale

#original forecast
error.orf.nnet=array(dim=c(station_number,error_number,time_number))
error.orf.ets=array(dim=c(station_number,error_number,time_number))
error.orf.arima=array(dim=c(station_number,error_number,time_number))

#HLS reconcile
error.hls.nnet=array(dim=c(station_number,error_number,time_number))
error.hls.ets=array(dim=c(station_number,error_number,time_number))
error.hls.arima=array(dim=c(station_number,error_number,time_number))

#WLS reconcile
error.wls.nnet=array(dim=c(station_number,error_number,time_number))
error.wls.ets=array(dim=c(station_number,error_number,time_number))
error.wls.arima=array(dim=c(station_number,error_number,time_number))
#OLS reconcile
error.ols.nnet=array(dim=c(station_number,error_number,time_number))
error.ols.ets=array(dim=c(station_number,error_number,time_number))
error.ols.arima=array(dim=c(station_number,error_number,time_number))
#BU reconcile
error.bu.nnet=array(dim=c(station_number,error_number,time_number))
error.bu.ets=array(dim=c(station_number,error_number,time_number))
error.bu.arima=array(dim=c(station_number,error_number,time_number))


for (ii in seq_along(station)){
  for (jj in seq_along(time)){
    Y<-test_observ[[ii]][[jj]]
    
    error.orf.nnet[ii,,jj]<-c(RMSE(nnet_orf[[ii]][[jj]],Y),
                              NRMSE(nnet_orf[[ii]][[jj]],Y),MAE(nnet_orf[[ii]][[jj]],Y),
                              NMAE(nnet_orf[[ii]][[jj]],Y),R2(nnet_orf[[ii]][[jj]],Y),mape(Y,nnet_orf[[ii]][[jj]]),
                              mase(Y,nnet_orf[[ii]][[jj]]),smape(Y,nnet_orf[[ii]][[jj]]),percent_bias(Y,nnet_orf[[ii]][[jj]]),
                              KGE(nnet_orf[[ii]][[jj]],Y),NASH(nnet_orf[[ii]][[jj]],Y))
    
    error.orf.ets[ii,,jj]<-c(RMSE(ets_orf[[ii]][[jj]],Y),
                             NRMSE(ets_orf[[ii]][[jj]],Y),MAE(ets_orf[[ii]][[jj]],Y),
                             NMAE(ets_orf[[ii]][[jj]],Y),R2(ets_orf[[ii]][[jj]],Y),
                             mape(Y,ets_orf[[ii]][[jj]]),
                             mase(Y,ets_orf[[ii]][[jj]]),smape(Y,ets_orf[[ii]][[jj]]),percent_bias(Y,ets_orf[[ii]][[jj]]),
                             KGE(ets_orf[[ii]][[jj]],Y),NASH(ets_orf[[ii]][[jj]],Y))
    
    error.orf.arima[ii,,jj]<-c(RMSE(arima_orf[[ii]][[jj]],Y),
                               NRMSE(arima_orf[[ii]][[jj]],Y),MAE(arima_orf[[ii]][[jj]],Y),
                               NMAE(arima_orf[[ii]][[jj]],Y),R2(arima_orf[[ii]][[jj]],Y),
                               mape(Y,arima_orf[[ii]][[jj]]),
                               mase(Y,arima_orf[[ii]][[jj]]),smape(Y,arima_orf[[ii]][[jj]]),percent_bias(Y,arima_orf[[ii]][[jj]]),
                               KGE(arima_orf[[ii]][[jj]],Y),NASH(arima_orf[[ii]][[jj]],Y))
    
    
    error.hls.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[1]][[jj]],Y),
                              NRMSE(nnet_rec[[ii]][[1]][[jj]],Y),MAE(nnet_rec[[ii]][[1]][[jj]],Y),
                              NMAE(nnet_rec[[ii]][[1]][[jj]],Y),R2(nnet_rec[[ii]][[1]][[jj]],Y),
                              mape(Y,nnet_rec[[ii]][[1]][[jj]]),
                              mase(Y,nnet_rec[[ii]][[1]][[jj]]),smape(Y,nnet_rec[[ii]][[1]][[jj]]),
                              percent_bias(Y,nnet_rec[[ii]][[1]][[jj]]),
                              KGE(nnet_rec[[ii]][[1]][[jj]],Y),NASH(nnet_rec[[ii]][[1]][[jj]],Y))
    
    error.hls.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[1]][[jj]],Y),
                             NRMSE(ets_rec[[ii]][[1]][[jj]],Y),MAE(ets_rec[[ii]][[1]][[jj]],Y),
                             NMAE(ets_rec[[ii]][[1]][[jj]],Y),R2(ets_rec[[ii]][[1]][[jj]],Y),
                             mape(Y,ets_rec[[ii]][[1]][[jj]]),
                             mase(Y,ets_rec[[ii]][[1]][[jj]]),smape(Y,ets_rec[[ii]][[1]][[jj]]),
                             percent_bias(Y,ets_rec[[ii]][[1]][[jj]]),
                             KGE(ets_rec[[ii]][[1]][[jj]],Y),NASH(ets_rec[[ii]][[1]][[jj]],Y))
    
    error.hls.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[1]][[jj]],Y),
                               NRMSE(arima_rec[[ii]][[1]][[jj]],Y),MAE(arima_rec[[ii]][[1]][[jj]],Y),
                               NMAE(arima_rec[[ii]][[1]][[jj]],Y),R2(arima_rec[[ii]][[1]][[jj]],Y),
                               mape(Y,arima_rec[[ii]][[1]][[jj]]),
                               mase(Y,arima_rec[[ii]][[1]][[jj]]),smape(Y,arima_rec[[ii]][[1]][[jj]]),
                               percent_bias(Y,arima_rec[[ii]][[1]][[jj]]),
                               KGE(arima_rec[[ii]][[1]][[jj]],Y),NASH(arima_rec[[ii]][[1]][[jj]],Y))
    
    
    error.wls.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[2]][[jj]],Y),
                              NRMSE(nnet_rec[[ii]][[2]][[jj]],Y),MAE(nnet_rec[[ii]][[2]][[jj]],Y),
                              NMAE(nnet_rec[[ii]][[2]][[jj]],Y),R2(nnet_rec[[ii]][[2]][[jj]],Y),
                              mape(Y,nnet_rec[[ii]][[2]][[jj]]),
                              mase(Y,nnet_rec[[ii]][[2]][[jj]]),smape(Y,nnet_rec[[ii]][[2]][[jj]]),
                              percent_bias(Y,nnet_rec[[ii]][[2]][[jj]]),
                              KGE(nnet_rec[[ii]][[2]][[jj]],Y),NASH(arima_rec[[ii]][[1]][[jj]],Y))
    
    error.wls.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[2]][[jj]],Y),
                             NRMSE(ets_rec[[ii]][[2]][[jj]],Y),MAE(ets_rec[[ii]][[2]][[jj]],Y),
                             NMAE(ets_rec[[ii]][[2]][[jj]],Y),R2(ets_rec[[ii]][[2]][[jj]],Y),
                             mape(Y,ets_rec[[ii]][[2]][[jj]]),
                             mase(Y,ets_rec[[ii]][[2]][[jj]]),smape(Y,ets_rec[[ii]][[2]][[jj]]),
                             percent_bias(Y,ets_rec[[ii]][[2]][[jj]]),
                             KGE(ets_rec[[ii]][[2]][[jj]],Y),NASH(ets_rec[[ii]][[2]][[jj]],Y))
    
    error.wls.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[2]][[jj]],Y),
                               NRMSE(arima_rec[[ii]][[2]][[jj]],Y),MAE(arima_rec[[ii]][[2]][[jj]],Y),
                               NMAE(arima_rec[[ii]][[2]][[jj]],Y),R2(arima_rec[[ii]][[2]][[jj]],Y),
                               mape(Y,arima_rec[[ii]][[2]][[jj]]),
                               mase(Y,arima_rec[[ii]][[2]][[jj]]),smape(Y,arima_rec[[ii]][[2]][[jj]]),
                               percent_bias(Y,arima_rec[[ii]][[2]][[jj]]),
                               KGE(arima_rec[[ii]][[2]][[jj]],Y),NASH(arima_rec[[ii]][[2]][[jj]],Y))
    
    
    error.ols.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[3]][[jj]],Y),
                              NRMSE(nnet_rec[[ii]][[3]][[jj]],Y),MAE(nnet_rec[[ii]][[3]][[jj]],Y),
                              NMAE(nnet_rec[[ii]][[3]][[jj]],Y),R2(nnet_rec[[ii]][[3]][[jj]],Y),
                              mape(Y,nnet_rec[[ii]][[3]][[jj]]),
                              mase(Y,nnet_rec[[ii]][[3]][[jj]]),smape(Y,nnet_rec[[ii]][[3]][[jj]]),
                              percent_bias(Y,nnet_rec[[ii]][[3]][[jj]]),
                              KGE(nnet_rec[[ii]][[3]][[jj]],Y),NASH(nnet_rec[[ii]][[3]][[jj]],Y))
    
    error.ols.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[3]][[jj]],Y),
                             NRMSE(ets_rec[[ii]][[3]][[jj]],Y),MAE(ets_rec[[ii]][[3]][[jj]],Y),
                             NMAE(ets_rec[[ii]][[3]][[jj]],Y),R2(ets_rec[[ii]][[3]][[jj]],Y),
                             mape(Y,ets_rec[[ii]][[3]][[jj]]),
                             mase(Y,ets_rec[[ii]][[3]][[jj]]),smape(Y,ets_rec[[ii]][[3]][[jj]]),
                             percent_bias(Y,ets_rec[[ii]][[3]][[jj]]),
                             KGE(ets_rec[[ii]][[3]][[jj]],Y),NASH(ets_rec[[ii]][[3]][[jj]],Y))
    
    error.ols.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[3]][[jj]],Y),
                               NRMSE(arima_rec[[ii]][[3]][[jj]],Y),MAE(arima_rec[[ii]][[3]][[jj]],Y),
                               NMAE(arima_rec[[ii]][[3]][[jj]],Y),R2(arima_rec[[ii]][[3]][[jj]],Y),
                               mape(Y,arima_rec[[ii]][[3]][[jj]]),
                               mase(Y,arima_rec[[ii]][[3]][[jj]]),smape(Y,arima_rec[[ii]][[3]][[jj]]),
                               percent_bias(Y,arima_rec[[ii]][[3]][[jj]]),
                               KGE(arima_rec[[ii]][[3]][[jj]],Y),NASH(arima_rec[[ii]][[3]][[jj]],Y))
    
    
    error.bu.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[4]][[jj]],Y),
                             NRMSE(nnet_rec[[ii]][[4]][[jj]],Y),MAE(nnet_rec[[ii]][[4]][[jj]],Y),
                             NMAE(nnet_rec[[ii]][[4]][[jj]],Y),R2(nnet_rec[[ii]][[4]][[jj]],Y),
                             mape(Y,nnet_rec[[ii]][[4]][[jj]]),
                             mase(Y,nnet_rec[[ii]][[4]][[jj]]),smape(Y,nnet_rec[[ii]][[4]][[jj]]),
                             percent_bias(Y,nnet_rec[[ii]][[4]][[jj]]),
                             KGE(nnet_rec[[ii]][[4]][[jj]],Y),NASH(nnet_rec[[ii]][[4]][[jj]],Y))
    
    error.bu.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[4]][[jj]],Y),
                            NRMSE(ets_rec[[ii]][[4]][[jj]],Y),MAE(ets_rec[[ii]][[4]][[jj]],Y),
                            NMAE(ets_rec[[ii]][[4]][[jj]],Y),R2(ets_rec[[ii]][[4]][[jj]],Y),
                            mape(Y,ets_rec[[ii]][[4]][[jj]]),
                            mase(Y,ets_rec[[ii]][[4]][[jj]]),smape(Y,ets_rec[[ii]][[4]][[jj]]),
                            percent_bias(Y,ets_rec[[ii]][[4]][[jj]]),
                            KGE(ets_rec[[ii]][[4]][[jj]],Y),NASH(ets_rec[[ii]][[4]][[jj]],Y))
    
    error.bu.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[4]][[jj]],Y),
                              NRMSE(arima_rec[[ii]][[4]][[jj]],Y),MAE(arima_rec[[ii]][[4]][[jj]],Y),
                              NMAE(arima_rec[[ii]][[4]][[jj]],Y),R2(arima_rec[[ii]][[4]][[jj]],Y),
                              mape(Y,arima_rec[[ii]][[4]][[jj]]),
                              mase(Y,arima_rec[[ii]][[4]][[jj]]),smape(Y,arima_rec[[ii]][[4]][[jj]]),
                              percent_bias(Y,arima_rec[[ii]][[4]][[jj]]),
                              KGE(arima_rec[[ii]][[4]][[jj]],Y),NASH(arima_rec[[ii]][[4]][[jj]],Y))
    
  }
}


# Filtering ---------------------------------------------------------------
# filtering using R2 (Nash). Only considering the stations with monthly...Yearly (1 to 6) R2 greater than 0
filter_arima_x<-which(error.orf.arima[,5,filtering_scale]>0)
if (length(filter_arima_x)>1){
  error.orf.arima.filter<-error.orf.arima[filter_arima_x,,]
  error.hls.arima.filter<-error.hls.arima[filter_arima_x,,]
  error.wls.arima.filter<-error.wls.arima[filter_arima_x,,]
  error.ols.arima.filter<-error.ols.arima[filter_arima_x,,]
  error.bu.arima.filter<-error.bu.arima[filter_arima_x,,]
} else {
  
  error.orf.arima.filter<-array(error.orf.arima[filter_arima_x,,],dim=c(1,error_number,time_number))
  error.hls.arima.filter<-array(error.hls.arima[filter_arima_x,,],dim=c(1,error_number,time_number))
  error.wls.arima.filter<-array(error.wls.arima[filter_arima_x,,],dim=c(1,error_number,time_number))
  error.ols.arima.filter<-array(error.ols.arima[filter_arima_x,,],dim=c(1,error_number,time_number))
  error.bu.arima.filter<-array(error.bu.arima[filter_arima_x,,],dim=c(1,error_number,time_number))
}


filter_nnet_x<-which(error.orf.nnet[,5,filtering_scale]>0)
if (length(filter_nnet_x)>1){
  error.orf.nnet.filter<-error.orf.nnet[filter_nnet_x,,]
  error.hls.nnet.filter<-error.hls.nnet[filter_nnet_x,,]
  error.wls.nnet.filter<-error.wls.nnet[filter_nnet_x,,]
  error.ols.nnet.filter<-error.ols.nnet[filter_nnet_x,,]
  error.bu.nnet.filter<-error.bu.nnet[filter_nnet_x,,]
} else{
  
  error.orf.nnet.filter<-array(error.orf.nnet[filter_nnet_x,,],dim=c(1,error_number,time_number))
  error.hls.nnet.filter<-array(error.hls.nnet[filter_nnet_x,,],dim=c(1,error_number,time_number))
  error.wls.nnet.filter<-array(error.wls.nnet[filter_nnet_x,,],dim=c(1,error_number,time_number))
  error.ols.nnet.filter<-array(error.ols.nnet[filter_nnet_x,,],dim=c(1,error_number,time_number))
  error.bu.nnet.filter<-array(error.bu.nnet[filter_nnet_x,,],dim=c(1,error_number,time_number))
}

filter_ets_x<-which(error.orf.ets[,5,filtering_scale]>0)
if (length(filter_ets_x)>1){
  error.orf.ets.filter<-error.orf.ets[filter_ets_x,,]
  error.hls.ets.filter<-error.hls.ets[filter_ets_x,,]
  error.wls.ets.filter<-error.wls.ets[filter_ets_x,,]
  error.ols.ets.filter<-error.ols.ets[filter_ets_x,,]
  error.bu.ets.filter<-error.bu.ets[filter_ets_x,,]
} else{
  error.orf.ets.filter<-array(error.orf.ets[filter_ets_x,,],dim=c(1,error_number,time_number))
  error.hls.ets.filter<-array(error.hls.ets[filter_ets_x,,],dim=c(1,error_number,time_number))
  error.wls.ets.filter<-array(error.wls.ets[filter_ets_x,,],dim=c(1,error_number,time_number))
  error.ols.ets.filter<-array(error.ols.ets[filter_ets_x,,],dim=c(1,error_number,time_number))
  error.bu.ets.filter<-array(error.bu.ets[filter_ets_x,,],dim=c(1,error_number,time_number))
}





REL_ERROR<-function(X1,X2){
  return((X1-X2)/(X2)*100)
}

# sorting by nrmse of the filter
order_arima<-order(error.orf.arima.filter[,2,filtering_scale])
if (length(order_arima)>1){
  error.orf.arima.filter.sort<-error.orf.arima.filter[order_arima,,]
  error.hls.arima.filter.sort<-error.hls.arima.filter[order_arima,,]
  error.wls.arima.filter.sort<-error.wls.arima.filter[order_arima,,]
  error.ols.arima.filter.sort<-error.ols.arima.filter[order_arima,,]
  error.bu.arima.filter.sort<-error.bu.arima.filter[order_arima,,]
} else {
  error.orf.arima.filter.sort<-array(error.orf.arima.filter[order_arima,,],dim=c(1,error_number,time_number))
  error.hls.arima.filter.sort<-array(error.hls.arima.filter[order_arima,,],dim=c(1,error_number,time_number))
  error.wls.arima.filter.sort<-array(error.wls.arima.filter[order_arima,,],dim=c(1,error_number,time_number))
  error.ols.arima.filter.sort<-array(error.ols.arima.filter[order_arima,,],dim=c(1,error_number,time_number))
  error.bu.arima.filter.sort<-array(error.bu.arima.filter[order_arima,,],dim=c(1,error_number,time_number))
}
order_nnet<-order(error.orf.nnet.filter[,2,filtering_scale])
if (length(order_nnet)>1){
  error.orf.nnet.filter.sort<-error.orf.nnet.filter[order_nnet,,]
  error.hls.nnet.filter.sort<-error.hls.nnet.filter[order_nnet,,]
  error.wls.nnet.filter.sort<-error.wls.nnet.filter[order_nnet,,]
  error.ols.nnet.filter.sort<-error.ols.nnet.filter[order_nnet,,]
  error.bu.nnet.filter.sort<-error.bu.nnet.filter[order_nnet,,]
} else{
  error.orf.nnet.filter.sort<-array(error.orf.nnet.filter[order_nnet,,],dim=c(1,error_number,time_number))
  error.hls.nnet.filter.sort<-array(error.hls.nnet.filter[order_nnet,,],dim=c(1,error_number,time_number))
  error.wls.nnet.filter.sort<-array(error.wls.nnet.filter[order_nnet,,],dim=c(1,error_number,time_number))
  error.ols.nnet.filter.sort<-array(error.ols.nnet.filter[order_nnet,,],dim=c(1,error_number,time_number))
  error.bu.nnet.filter.sort<-array(error.bu.nnet.filter[order_nnet,,],dim=c(1,error_number,time_number))
}
order_ets<-order(error.orf.ets.filter[,2,filtering_scale])
if (length(order_ets)>1){
  error.orf.ets.filter.sort<-error.orf.ets.filter[order_ets,,]
  error.hls.ets.filter.sort<-error.hls.ets.filter[order_ets,,]
  error.wls.ets.filter.sort<-error.wls.ets.filter[order_ets,,]
  error.ols.ets.filter.sort<-error.ols.ets.filter[order_ets,,]
  error.bu.ets.filter.sort<-error.bu.ets.filter[order_ets,,]
} else {
  error.orf.ets.filter.sort<-array(error.orf.ets.filter[order_ets,,],dim=c(1,error_number,time_number))
  error.hls.ets.filter.sort<-array(error.hls.ets.filter[order_ets,,],dim=c(1,error_number,time_number))
  error.wls.ets.filter.sort<-array(error.wls.ets.filter[order_ets,,],dim=c(1,error_number,time_number))
  error.ols.ets.filter.sort<-array(error.ols.ets.filter[order_ets,,],dim=c(1,error_number,time_number))
  error.bu.ets.filter.sort<-array(error.bu.ets.filter[order_ets,,],dim=c(1,error_number,time_number))
}

arima_mean_error<-vector('list',length =5)


arima_error_whole<-list(error.orf.arima.filter.sort,error.hls.arima.filter.sort,error.wls.arima.filter.sort,
                        error.ols.arima.filter.sort,error.bu.arima.filter.sort)
ets_error_whole<-list(error.orf.ets.filter.sort,error.hls.ets.filter.sort,error.wls.ets.filter.sort,
                      error.ols.ets.filter.sort,error.bu.ets.filter.sort)
nnet_error_whole<-list(error.orf.nnet.filter.sort,error.hls.nnet.filter.sort,error.wls.nnet.filter.sort,
                       error.ols.nnet.filter.sort,error.bu.nnet.filter.sort)

error.arima.whole.ave<-vector('list',length =5)
error.ets.whole.ave<-vector('list',length =5)
error.nnet.whole.ave<-vector('list',length =5)


for (jj in seq(1,5)){
  temp_nnet<-matrix(nrow=length(time),ncol=length(error_terms))
  temp_ets<-matrix(nrow=length(time),ncol=length(error_terms))
  temp_arima<-matrix(nrow=length(time),ncol=length(error_terms))
  for (ii in seq_along(time)){
    data<-nnet_error_whole[[jj]][,,ii]
    if (is.null(dim(data))){
      temp_nnet[ii,]<-apply(as.matrix(data,dim=c(1,length(data))),1,mean, na.rm=TRUE)
    } else{
      temp_nnet[ii,]<-apply(data,2,mean, na.rm=TRUE)
    }
    data<-ets_error_whole[[jj]][,,ii]
    if (is.null(dim(data))){
      temp_ets[ii,]<-apply(as.matrix(data,dim=c(1,length(data))),1,mean, na.rm=TRUE)
    } else{
      temp_ets[ii,]<-apply(data,2,mean,na.rm=TRUE)
    }
    data<-arima_error_whole[[jj]][,,ii]
    if (is.null(dim(data))){
      temp_arima[ii,]<-apply(as.matrix(data,dim=c(1,length(data))),1,mean, na.rm=TRUE)
    } else{
      temp_arima[ii,]<-apply(data,2,mean,na.rm=TRUE)
    }
    
  }
  error.arima.whole.ave[[jj]]<-temp_arima
  error.ets.whole.ave[[jj]]<-temp_ets
  error.nnet.whole.ave[[jj]]<-temp_nnet
}