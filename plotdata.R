quartz()

#turns on the pdf encoder
pdf(encoding="ISOLatin2",height=8, width=12)
i <- 1

#set the number of (rows,columns) for the plots
#par(mfcol=c(5,1))

### ---------------------------------------------------------------
##set window for plot for each page of PDF file, currently set to 1770 as this is the average points sampled per day in the current system. 
### there should be 1440 samples a day, but bug in current Delphi and hardware and it samples 1770 times a day on average. THIS NEEDS TO BE FIXED!!!!!

inc <- 1440*7

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
  i <- i+ inc
}

dev.off()

