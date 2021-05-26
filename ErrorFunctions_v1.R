# functions for calculating error parameters
# pr is the prediction set, ewhile Y is the observation set
# pr, and Y could be timeseries
# res is the residual vector
RMSE<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  rmse<-(mean(res^2,na.rm =T))^0.5
  return(rmse)
}
NASH<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  nash<-1-(sum((res)^2,na.rm =T)/sum((y.vec-mean(y.vec,na.rm =T))^2,na.rm =T))
  return(nash)
}
ME<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  me<-mean(res,na.rm =T)
  return(me)
}
MAE<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  mae<-mean(abs(res),na.rm = T)
  return(mae)
}
NRMSE<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  rmse<-(mean(res^2,na.rm =T))^0.5
  nrmse<-rmse/mean(y.vec,na.rm =T)
  return(nrmse)
}
NMAE<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  mae<-mean(abs(res),na.rm = T)
  nmae<-mae/mean(y.vec,na.rm =T)
  return(nmae)
}
# percentage mean bias/error
PME<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  pme<-(sum(res,na.rm =T))/(sum(y.vec,na.rm =T))*100
  return(pme)
}
# sum square of model
SSM<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  ssm<-sum((pr.vec-mean(y.vec,na.rm =T))^2,na.rm =T)
return(ssm)
}
# sum square total
SST<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  sst<-sum((y.vec-mean(y.vec,na.rm =T))^2,na.rm =T)
  return(sst)
}
# sum square residual
SSR<-function(pr,Y){
  pr.vec<-as.vector(pr)
  y.vec<-as.vector(Y)
  res<-y.vec-pr.vec
  ssr<-sum((res)^2,na.rm =T)
  return(ssr)
}

R2<-function(pr,Y){
  r2<-1-SSR(pr,Y)/SST(pr,Y)
  return(r2)
}






