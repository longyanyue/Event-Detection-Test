i <- 50
limit<-dim(dataset_pH)[1]
upbounds<-0
lowbounds<-0
eventimpact<-2000
while(i<=limit){
  if(as.numeric(dataset_pH$pH[i]) > trendline[i,2]){
    rate<-abs(as.numeric(dataset_pH$pH[i])-trendline[i,2])/trendline[i,2]
    upbounds<-upbounds+20*rate
    lowbounds<-lowbounds-20*rate
    if(lowbounds<0)
      lowbounds<-0
  }
  else if(as.numeric(dataset_pH$pH[i]) < trendline[i,1]){
    rate<-abs(as.numeric(dataset_pH$pH[i])-trendline[i,1])/trendline[i,1]
    lowbounds<-lowbounds+20*rate
    upbounds<-upbounds-20*rate
    if(upbounds<0)
      upbounds<-0
  }
  else{
    lowbounds<-lowbounds-80
    upbounds<-upbounds-80
    if(lowbounds<0)
      lowbounds<-0
    if(upbounds<0)
      upbounds<-0
  }
  if(upbounds>=eventimpact){
    points(i-50,as.numeric(dataset_pH$pH[i-50]),col="yellow")
  }
  else if(lowbounds>=eventimpact){
    points(i-50,as.numeric(dataset_pH$pH[i-50]),col="green")
  }
  i<-i+1
}