#Only monthly, two monthly, and 4 monthly are considered

# compared to version 1, more error metrics are added. It is noted that similar to version 1,
# R2 values smaller than 0 are filtered.


# this script is for post/error analysis of thief results
rm(list=ls())
# setting working directory
setwd('D:/University/Paper/Quilty_precipitation/Canopex/Code_NewHorizon/')
# loading data
# loading the forecasted data from thief
# the output is thief_list
load("nnet_new_thief_reconciled_v1.RData")
nnet_rec<-thief_list
load("ets_new_thief_reconciled_v1.RData")
ets_rec<-thief_list
load("arima_new_thief_reconciled_v1.RData")
arima_rec<-thief_list
rm(thief_list)
# loading the test set for comparision
# the output is test listi
load("nnet_thief_test_all.RData")
test_observ<-test_listi
rm(test_listi)
for (ii in seq_along(test_observ)){
  test_observ[[ii]]<-test_observ[[ii]][c(1,2,4)]
}
# loading the original forecast
# the output is prediction listi
load("nnet_thief_prediction_all.RData")
nnet_orf<-Prediction_listi
load("ets_thief_prediction_all.RData")
ets_orf<-Prediction_listi
load("arima_thief_prediction_all.RData")
arima_orf<-Prediction_listi
rm(Prediction_listi)
for (ii in seq_along(test_observ)){
  arima_orf[[ii]]<-arima_orf[[ii]][c(1,2,4)]
  ets_orf[[ii]]<-ets_orf[[ii]][c(1,2,4)]
  nnet_orf[[ii]]<-nnet_orf[[ii]][c(1,2,4)]
}
#saving new horizon files
save(test_observ,file='TestData_NewHorizon.RData')
save(arima_orf,file='Arima_PredictionAll_NewHorizon.RData')
save(ets_orf,file='ETS_PredictionAll_NewHorizon.RData')
save(nnet_orf,file='NNET_PredictionAll_NewHorizon.RData')
# load error functions
source('ErrorFunctions_v1.R')
#load error library(Metrics)
library(Metrics)
#load library(hydroGOF)
library(hydroGOF)
station<-names(test_observ)
station_number<-length(test_observ)
error_terms<-c("RMSE",'NRMSE',"MAE",'NMAE','R2','KGE')
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
        NMAE(nnet_orf[[ii]][[jj]],Y),R2(nnet_orf[[ii]][[jj]],Y),
        KGE(nnet_orf[[ii]][[jj]],Y))
    
    error.orf.ets[ii,,jj]<-c(RMSE(ets_orf[[ii]][[jj]],Y),
        NRMSE(ets_orf[[ii]][[jj]],Y),MAE(ets_orf[[ii]][[jj]],Y),
        NMAE(ets_orf[[ii]][[jj]],Y),R2(ets_orf[[ii]][[jj]],Y),
        KGE(ets_orf[[ii]][[jj]],Y))
    
    error.orf.arima[ii,,jj]<-c(RMSE(arima_orf[[ii]][[jj]],Y),
        NRMSE(arima_orf[[ii]][[jj]],Y),MAE(arima_orf[[ii]][[jj]],Y),
        NMAE(arima_orf[[ii]][[jj]],Y),R2(arima_orf[[ii]][[jj]],Y),
        KGE(arima_orf[[ii]][[jj]],Y))
    
    
    error.hls.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[1]][[jj]],Y),
        NRMSE(nnet_rec[[ii]][[1]][[jj]],Y),MAE(nnet_rec[[ii]][[1]][[jj]],Y),
        NMAE(nnet_rec[[ii]][[1]][[jj]],Y),R2(nnet_rec[[ii]][[1]][[jj]],Y),
        KGE(nnet_rec[[ii]][[1]][[jj]],Y))
    
    error.hls.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[1]][[jj]],Y),
        NRMSE(ets_rec[[ii]][[1]][[jj]],Y),MAE(ets_rec[[ii]][[1]][[jj]],Y),
        NMAE(ets_rec[[ii]][[1]][[jj]],Y),R2(ets_rec[[ii]][[1]][[jj]],Y),
        KGE(ets_rec[[ii]][[1]][[jj]],Y))
    
    error.hls.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[1]][[jj]],Y),
        NRMSE(arima_rec[[ii]][[1]][[jj]],Y),MAE(arima_rec[[ii]][[1]][[jj]],Y),
        NMAE(arima_rec[[ii]][[1]][[jj]],Y),R2(arima_rec[[ii]][[1]][[jj]],Y),
        KGE(arima_rec[[ii]][[1]][[jj]],Y))
    
    
    
    error.ols.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[2]][[jj]],Y),
          NRMSE(nnet_rec[[ii]][[2]][[jj]],Y),MAE(nnet_rec[[ii]][[2]][[jj]],Y),
          NMAE(nnet_rec[[ii]][[2]][[jj]],Y),R2(nnet_rec[[ii]][[2]][[jj]],Y),
          KGE(nnet_rec[[ii]][[2]][[jj]],Y))
    
    error.ols.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[2]][[jj]],Y),
          NRMSE(ets_rec[[ii]][[2]][[jj]],Y),MAE(ets_rec[[ii]][[2]][[jj]],Y),
          NMAE(ets_rec[[ii]][[2]][[jj]],Y),R2(ets_rec[[ii]][[2]][[jj]],Y),
          KGE(ets_rec[[ii]][[2]][[jj]],Y))
    
    error.ols.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[2]][[jj]],Y),
          NRMSE(arima_rec[[ii]][[2]][[jj]],Y),MAE(arima_rec[[ii]][[2]][[jj]],Y),
          NMAE(arima_rec[[ii]][[2]][[jj]],Y),R2(arima_rec[[ii]][[2]][[jj]],Y),
          KGE(arima_rec[[ii]][[2]][[jj]],Y))
    
    
    error.bu.nnet[ii,,jj]<-c(RMSE(nnet_rec[[ii]][[3]][[jj]],Y),
          NRMSE(nnet_rec[[ii]][[3]][[jj]],Y),MAE(nnet_rec[[ii]][[3]][[jj]],Y),
          NMAE(nnet_rec[[ii]][[3]][[jj]],Y),R2(nnet_rec[[ii]][[3]][[jj]],Y),
          KGE(nnet_rec[[ii]][[3]][[jj]],Y))
    
    error.bu.ets[ii,,jj]<-c(RMSE(ets_rec[[ii]][[3]][[jj]],Y),
          NRMSE(ets_rec[[ii]][[3]][[jj]],Y),MAE(ets_rec[[ii]][[3]][[jj]],Y),
          NMAE(ets_rec[[ii]][[3]][[jj]],Y),R2(ets_rec[[ii]][[3]][[jj]],Y),
          KGE(ets_rec[[ii]][[3]][[jj]],Y))
    
    error.bu.arima[ii,,jj]<-c(RMSE(arima_rec[[ii]][[3]][[jj]],Y),
          NRMSE(arima_rec[[ii]][[3]][[jj]],Y),MAE(arima_rec[[ii]][[3]][[jj]],Y),
          NMAE(arima_rec[[ii]][[3]][[jj]],Y),R2(arima_rec[[ii]][[3]][[jj]],Y),
          KGE(arima_rec[[ii]][[3]][[jj]],Y))
  }
}

# filtering using R2. Only considering the stations with monthly r2 greater than 0
error.orf.arima.filter<-error.orf.arima[which(error.orf.arima[,5,1]>0),,]
error.hls.arima.filter<-error.hls.arima[which(error.orf.arima[,5,1]>0),,]
error.ols.arima.filter<-error.ols.arima[which(error.orf.arima[,5,1]>0),,]
error.bu.arima.filter<-error.bu.arima[which(error.orf.arima[,5,1]>0),,]

error.orf.nnet.filter<-error.orf.nnet[which(error.orf.nnet[,5,1]>0),,]
error.hls.nnet.filter<-error.hls.nnet[which(error.orf.nnet[,5,1]>0),,]
error.ols.nnet.filter<-error.ols.nnet[which(error.orf.nnet[,5,1]>0),,]
error.bu.nnet.filter<-error.bu.nnet[which(error.orf.nnet[,5,1]>0),,]


error.orf.ets.filter<-error.orf.ets[which(error.orf.ets[,5,1]>0),,]
error.hls.ets.filter<-error.hls.ets[which(error.orf.ets[,5,1]>0),,]
error.ols.ets.filter<-error.ols.ets[which(error.orf.ets[,5,1]>0),,]
error.bu.ets.filter<-error.bu.ets[which(error.orf.ets[,5,1]>0),,]


REL_ERROR<-function(X1,X2){
  return((X1-X2)/(X2)*100)
}

# sorting by nrmse
error.orf.arima.filter.sort<-error.orf.arima.filter[order(error.orf.arima.filter[,2,1]),,]
error.hls.arima.filter.sort<-error.hls.arima.filter[order(error.orf.arima.filter[,2,1]),,]
error.ols.arima.filter.sort<-error.ols.arima.filter[order(error.orf.arima.filter[,2,1]),,]
error.bu.arima.filter.sort<-error.bu.arima.filter[order(error.orf.arima.filter[,2,1]),,]

error.orf.nnet.filter.sort<-error.orf.nnet.filter[order(error.orf.nnet.filter[,2,1]),,]
error.hls.nnet.filter.sort<-error.hls.nnet.filter[order(error.orf.nnet.filter[,2,1]),,]
error.ols.nnet.filter.sort<-error.ols.nnet.filter[order(error.orf.nnet.filter[,2,1]),,]
error.bu.nnet.filter.sort<-error.bu.nnet.filter[order(error.orf.nnet.filter[,2,1]),,]

error.orf.ets.filter.sort<-error.orf.ets.filter[order(error.orf.ets.filter[,2,1]),,]
error.hls.ets.filter.sort<-error.hls.ets.filter[order(error.orf.ets.filter[,2,1]),,]
error.ols.ets.filter.sort<-error.ols.ets.filter[order(error.orf.ets.filter[,2,1]),,]
error.bu.ets.filter.sort<-error.bu.ets.filter[order(error.orf.ets.filter[,2,1]),,]

arima_mean_error<-vector('list',length =4)


arima_error_whole<-list(error.orf.arima.filter.sort,error.hls.arima.filter.sort,
                        error.ols.arima.filter.sort,error.bu.arima.filter.sort)
ets_error_whole<-list(error.orf.ets.filter.sort,error.hls.ets.filter.sort,
                        error.ols.ets.filter.sort,error.bu.ets.filter.sort)
nnet_error_whole<-list(error.orf.nnet.filter.sort,error.hls.nnet.filter.sort,
                      error.ols.nnet.filter.sort,error.bu.nnet.filter.sort)

error.arima.whole.ave<-vector('list',length =4)
error.ets.whole.ave<-vector('list',length =4)
error.nnet.whole.ave<-vector('list',length =4)


for (jj in seq(1,4)){
  temp_nnet<-matrix(nrow=length(time),ncol=length(error_terms))
  temp_ets<-matrix(nrow=length(time),ncol=length(error_terms))
  temp_arima<-matrix(nrow=length(time),ncol=length(error_terms))
  for (ii in seq_along(time)){
    temp_nnet[ii,]<-apply(nnet_error_whole[[jj]][,,ii],2,mean, na.rm=TRUE)
    temp_ets[ii,]<-apply(ets_error_whole[[jj]][,,ii],2,mean,na.rm=TRUE)
    temp_arima[ii,]<-apply(arima_error_whole[[jj]][,,ii],2,mean,na.rm=TRUE)
  }
  error.arima.whole.ave[[jj]]<-temp_arima
  error.ets.whole.ave[[jj]]<-temp_ets
  error.nnet.whole.ave[[jj]]<-temp_nnet
}


comb_method<-c('ORF','HLS','OLS','BU')
# change ets, arima, and nnet
for (ii in seq_along(error.arima.whole.ave)){
  rownames(error.arima.whole.ave[[ii]])<-time
  #change here
  df<-data.frame(error.nnet.whole.ave[[ii]])
  colnames(df)<-error_terms
  #change here
  file<-sprintf('error_nnet_NewHorizon%s_v2.csv',comb_method[ii])
  write.csv(df,file =file,sep = ',')
}


#comparing with ordinary forecast
ets_error_whole_rel<-lapply(ets_error_whole[-1],REL_ERROR,ets_error_whole[[1]])
nnet_error_whole_rel<-lapply(nnet_error_whole[-1],REL_ERROR,nnet_error_whole[[1]])
arima_error_whole_rel<-lapply(arima_error_whole[-1],REL_ERROR,arima_error_whole[[1]])

#comparing with ordinary forecast
ave_ets_error_whole_rel<-lapply(error.ets.whole.ave[-1],REL_ERROR,error.ets.whole.ave[[1]])
ave_nnet_error_whole_rel<-lapply(error.nnet.whole.ave[-1],REL_ERROR,error.nnet.whole.ave[[1]])
ave_arima_error_whole_rel<-lapply(error.arima.whole.ave[-1],REL_ERROR,error.arima.whole.ave[[1]])

error.arima.rel.ave<-vector('list',length =3)
error.ets.rel.ave<-vector('list',length =3)
error.nnet.rel.ave<-vector('list',length =3)

#we are saving the average relative change, not relative average change
#jj is the type of reconciliation
#nrow is time step
#ncol is error
for (jj in seq(1,3)){
  temp_nnet<-matrix(nrow=time_number,ncol=error_number)
  temp_ets<-matrix(nrow=time_number,ncol=error_number)
  temp_arima<-matrix(nrow=time_number,ncol=error_number)
  for (ii in seq_along(time)){
    temp_nnet[ii,]<-apply(nnet_error_whole_rel[[jj]][,,ii],2,mean,na.rm=TRUE)
    temp_ets[ii,]<-apply(ets_error_whole_rel[[jj]][,,ii],2,mean,na.rm=TRUE)
    temp_arima[ii,]<-apply(arima_error_whole_rel[[jj]][,,ii],2,mean,na.rm=TRUE)
  }
  error.arima.rel.ave[[jj]]<-temp_arima
  error.ets.rel.ave[[jj]]<-temp_ets
  error.nnet.rel.ave[[jj]]<-temp_nnet
}







comb_rel_method<-c('HLS','OLS','BU')
# change ets, arima, and nnet
for (ii in seq_along(error.ets.rel.ave)){
  rownames(error.ets.rel.ave[[ii]])<-time
  #change here
  df<-data.frame(error.arima.rel.ave[[ii]])
  colnames(df)<-error_terms
  #change here
  file<-sprintf('error_rel_arima_NewHorizon_%s_v2.csv',comb_rel_method[ii])
  write.csv(df,file =file,sep = ',')
}

save(error.arima.rel.ave,file ='error_arima_rel_ave_NewHorizon.RData')
save(error.nnet.rel.ave,file ='error_nnet_rel_ave_NewHorizon.RData')
save(error.ets.rel.ave,file ='error_ets_rel_ave_NewHorizon.RData')

save(ets_error_whole_rel,file ='error_ets_rel_NewHorizon.RData')
save(arima_error_whole_rel,file ='error_arima_rel_NewHorizon.RData')
save(nnet_error_whole_rel,file ='error_nnet_rel_NewHorizon.RData')
