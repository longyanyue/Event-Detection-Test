require(caTools)
plot(1:dim(dataset_pH)[1],dataset_pH$pH, type="l", ylab="pH(-)",xlab="",lwd=2, cex.lab=1,col="black",cex=1)
trendline = runquantile(dataset_pH$pH, 1440, probs=c(0.40, 0.6))
lines(1:dim(dataset_pH)[1],trendline[,1],col="red")
lines(1:dim(dataset_pH)[1],trendline[,2],col="blue")
i <- 51
limit<-dim(dataset_pH)[1]
upbounds<<-0
lowbounds<<-0
eventimpact<-100
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