quartz()

#turns on the pdf encoder
pdf(encoding="ISOLatin2",height=8, width=12)
i <- 1

#set the number of (rows,columns) for the plots
#par(mfcol=c(5,1))

### ---------------------------------------------------------------
##set window for plot for each page of PDF file, currently set to 1770 as this is the average points sampled per day in the current system. 
### there should be 1440 samples a day, but bug in current Delphi and hardware and it samples 1770 times a day on average. THIS NEEDS TO BE FIXED!!!!!

inc <- 1440

limit<-dim(dataset_pH)[1]
upbounds<<-0
lowbounds<<-0
m_upbounds<<-1.2
m_lowbounds<<-0.8
c_upbounds<<-1.4
c_lowbounds<<-0.6
m_weight <<- 5
c_weight <<- 10
eventimpact<-200

data.end <- dim(dataset_pH)[1]  ##change this to the length of the data set or where you want to plotting to stop.

while(i+inc <= (data.end)){
  
  sub<-(i):(i+(inc))
  
  plot(sub,dataset_pH$pH[sub], type="l", ylab="pH(-)",xlab="",lwd=2, cex.lab=1,col="black",cex=1)
  lines(sub,trendline[sub,1],col="red")
  lines(sub,trendline[sub,2],col="blue")
  j<-i
  while(j<=(i+inc)){
    if(as.numeric(dataset_pH$pH[j]) > trendline[j,2]){
      rate<-abs(as.numeric(dataset_pH$pH[j])-trendline[j,2])/trendline[j,2]
      upbounds<-upbounds+20*rate
      lowbounds<-lowbounds-20*rate
      if(as.numeric(dataset_pH$pH[j]) > m_upbounds*trendline[j,2]){
        upbounds<-upbounds+m_weight
        lowbounds<-lowbounds-m_weight
      }
      if(as.numeric(dataset_pH$pH[j]) > c_upbounds*trendline[j,2]){
        upbounds<-upbounds+c_weight
        lowbounds<-lowbounds-c_weight
      }
      if(lowbounds<0)
        lowbounds<-0
    }
    else if(as.numeric(dataset_pH$pH[j]) < trendline[j,1]){
      rate<-abs(as.numeric(dataset_pH$pH[j])-trendline[j,1])/trendline[j,1]
      lowbounds<-lowbounds+20*rate
      upbounds<-upbounds-20*rate
      if(as.numeric(dataset_pH$pH[j]) < m_lowbounds*trendline[j,1]){
        upbounds<-upbounds-m_weight
        lowbounds<-lowbounds+m_weight
      }
      if(as.numeric(dataset_pH$pH[j]) < c_lowbounds*trendline[j,1]){
        upbounds<-upbounds-c_weight
        lowbounds<-lowbounds+c_weight
      }
      if(upbounds<0)
        upbounds<-0
    }
    else{
      lowbounds<-lowbounds-100
      upbounds<-upbounds-100
      if(lowbounds<0)
        lowbounds<-0
      if(upbounds<0)
        upbounds<-0
    }
    if(upbounds>=eventimpact){
      points(j,as.numeric(dataset_pH$pH[j]),col="yellow")
    }
    else if(lowbounds>=eventimpact){
      points(j,as.numeric(dataset_pH$pH[j]),col="green")
    }
    j<-j+1
  }
  
  
  i <- i+ inc
}


dev.off()

