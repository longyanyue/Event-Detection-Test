library(GA)


eventimpact<-seq(100, 500, by = 10)
windowsize <- seq(120, 1440*2, by = 120)
highermark <- seq(0.51, 0.99, by = 0.01)
lowermark <- seq(0.01, 0.49, by = 0.01)
upthreshold <- seq(7.1, 9, by = 0.1)
lowthreshold <- seq(5, 6.9, by = 0.1)

Rastrigin <- function(eventimpact, windowsize,highermark,lowermark,upthreshold,lowthreshold)
{
  OEM <- c(4130:4700,10810:10920,14160:14260,15430:15650,36250:36740,37690:38100,60890:61040,64390:64520,65950:66050,
           67860:67960,68850:69100,80830:81000,134630:134690,81610:81950,83020:83180,84480:84630,87375:87460,
           88950:89150,90230:90430,90680:90780,95950:96050,104410:104580,111770:111850,113170:113250,116100:116380,
           123100:123290,131820:131980)
  datalength <- dim(dataset_pH)[1]
  upbounds<<-0
  lowbounds<<-0
  m_upbounds<<-1.2
  m_lowbounds<<-0.8
  c_upbounds<<-1.4
  c_lowbounds<<-0.6
  m_weight <<- 5
  c_weight <<- 10

  i <- 1
  markancher <- -1
  trendline <- matrix(, nrow = datalength, ncol = 2)
  eventmarks <- numeric(datalength)
  trendlinestack <- dataset_pH$pH[1:windowsize]
  trendline[1:windowsize,] <- runquantile(trendlinestack, windowsize, probs=c(lowermark, highermark))
  DEM <- c() #Detected Event Marks
  OinD <- 0
  DinO <- 0
  pOinD <- 0
  pDinO <- 0
  while((i+windowsize)<=datalength){
    temp_trendline <- runquantile(trendlinestack, windowsize, probs=c(lowermark, highermark))
    #####event detection process
    if((as.numeric(dataset_pH$pH[i+windowsize]) > trendline[i+windowsize-1,2]  || as.numeric(dataset_pH$pH[i+windowsize]) < trendline[i+windowsize-1,1]) && markancher < 0)
      markancher <- i+windowsize
    if(as.numeric(dataset_pH$pH[i+windowsize]) > trendline[i+windowsize-1,2] && as.numeric(dataset_pH$pH[i+windowsize]) > upthreshold){
      rate<-abs(as.numeric(dataset_pH$pH[i+windowsize])-trendline[i+windowsize-1,2])/trendline[i+windowsize-1,2]
      upbounds<-upbounds+10*rate
      lowbounds<-lowbounds-10*rate
      if(as.numeric(dataset_pH$pH[i+windowsize]) > m_upbounds*trendline[i+windowsize-1,2]){
        upbounds<-upbounds+m_weight
        lowbounds<-lowbounds-m_weight
      }
      if(as.numeric(dataset_pH$pH[i+windowsize]) > c_upbounds*trendline[i+windowsize-1,2]){
        upbounds<-upbounds+c_weight
        lowbounds<-lowbounds-c_weight
      }
      if(lowbounds<0)
        lowbounds<-0
    }
    else if(as.numeric(dataset_pH$pH[i+windowsize]) < trendline[i+windowsize-1,1] && as.numeric(dataset_pH$pH[i+windowsize]) < lowthreshold){
      rate<-abs(as.numeric(dataset_pH$pH[i+windowsize])-trendline[i+windowsize-1,1])/trendline[i+windowsize-1,1]
      lowbounds<-lowbounds+20*rate
      upbounds<-upbounds-20*rate    
      if(as.numeric(dataset_pH$pH[i+windowsize]) < m_lowbounds*trendline[i+windowsize-1,1]){
        upbounds<-upbounds-m_weight
        lowbounds<-lowbounds+m_weight
      }
      if(as.numeric(dataset_pH$pH[i+windowsize]) < c_lowbounds*trendline[i+windowsize-1,1]){
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
    if(as.numeric(dataset_pH$pH[i+windowsize]) >= trendline[i+windowsize-1,1] && as.numeric(dataset_pH$pH[i+windowsize]) <= trendline[i+windowsize-1,2])
      markancher <- -1
    if(upbounds>=eventimpact){
      if(markancher>0){
        eventmarks[markancher:(i+windowsize)] <- 1
        DEM <- c(DEM,(markancher:(i+windowsize)))
      }     
    }
    else if(lowbounds>=eventimpact){
      if(markancher>0){
        eventmarks[markancher:(i+windowsize)] <- -1
        DEM <- c(DEM,(markancher:(i+windowsize)))
      }   
    }
    trendline[i+windowsize,] <- temp_trendline[windowsize,]
    if(eventmarks[i+windowsize]==0){
      trendlinestack[1:(windowsize-1)] <- trendlinestack[2:windowsize]
      trendlinestack[windowsize] <- dataset_pH$pH[i+windowsize]
    }
    
    #  else
    #    trendline[i+windowsize,] <- c(mean(as.numeric(temp_trendline[(1:windowsize),1])),mean(as.numeric(temp_trendline[(1:windowsize),2])))
    i <- i+1
  }
  DEM <- unique(DEM)
  for(m in 1:length(DEM))
  {
    if(DEM[m] %in% OEM)
      DinO <- DinO+1
  }
  pDinO <- DinO/length(DEM)
  for(m in 1:length(OEM))
  {
    if(OEM[m] %in% DEM)
      OinD <- OinD+1
  }
  pOinD <- OinD/length(OEM)
  0.5*pDinO+0.5*pOinD
}

GA <- ga(type = "real-valued", fitness = function(x) +Rastrigin(x[1], x[2], x[3], x[4], x[5], x[6]),
         min = c(100,120,0.51,0.01,7.1,5), max = c(500,2880,0.99,0.49,9,6.9),
         popSize = 50, maxiter = 100)


summary(GA)
plot(GA)