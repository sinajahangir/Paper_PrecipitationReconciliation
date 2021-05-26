rm(list=ls())
# loading thief package
library("thief")
setwd('D:/University/Paper/Quilty_precipitation/Canopex/Code_NewHorizon')
# check names is set to true as we do not want to change the name of columns
data<-read.csv(file='Canopex_Monthly.csv',sep=',',header = T,check.names = FALSE)
# the name columns should change as space was used in the csv file
name_csv<-names(data)
names(data)<-gsub("'","",name_csv)
# convert data to timeseries
time_data<-lapply(data[-(1:2)],ts,start=c(min(data[[1]]), min(data[[2]])),frequency=12)
# selecting the stations from whole stations
testi<-time_data[[1]]
predi<-thief(testi,h=24,comb='mse',usemodel = 'ets')
