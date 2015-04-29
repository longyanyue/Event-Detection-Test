##### Simulate pH Data for Testing####

#####Parameters Pre-set
limit.max <<- 9
limit.min <<-5
base <<- 7
p.up <<- 0.5
p.down <<- 0.5
pH.interval <<- 0.1
p.interval <<- 0.01
randomchange <<- 0.05
#####Simulation Function
simul <- function(dataset, len){
  dataset[1] <<- 7
  for(i in 2:len){
    pH.int <- pH.interval*runif(1, 0.1, 1)
    rdm <- floor(runif(1, -1*p.down, p.up))
    if(rdm == 0)
      rdm <<- 1
    point <<- dataset[i-1]+pH.int*rdm
    if(point>limit.max || point<limit.min)
      dataset[i] <<- dataset[i-1]
    else
      dataset[i] <<- point
    p.up.temp <<- p.up+p.interval*(base-dataset[i])
    if(p.up.temp<1 && p.up.temp>0){
      p.up <<- p.up.temp
      p.down <<- 1-p.up
    } 
    timee <<- timee + 1
  }
}


#####Simulation Process
#####Parameters Pre-set
limit.max <<- 9
limit.min <<-5
base <<- 7
p.up <<- 0.5
p.down <<- 0.5
pH.interval <<- 0.02
p.interval <<- 0.0005
randomchange <<- 0.1
dataset <<- numeric(10000)
#simul(dataset, 3)
eventmarks <<- numeric(10000)
j <<- 1
while(j <= 10000){
  inc <- floor(runif(1,100,200))
  mark <- floor(runif(1,0.1,1.9))
  if(j+inc <= 10000)
    eventmarks[j:(j+inc)] <- mark
  if(j+inc > 10000)
    eventmarks[j:10000] <- mark
  j <- j+inc+1
}

limit.max2 <<- 7.5
limit.min2 <<-6.5
p.up2 <<- 0.5
p.down2 <<- 0.5
pH.interval2 <<- 0.02
p.interval2 <<- 0.0005

dataset[1] <- 7
for(i in 2:10000){
  if(eventmarks[i]==0){
    pH.int <- pH.interval2*runif(1, 0.1, 1)
    rdm <- floor(runif(1, -1*p.down2, p.up2))
    if(rdm == 0)
      rdm <- 1
    point2 <- dataset[i-1]+pH.int*rdm
    if(point2>limit.max2 || point2<limit.min2)
      dataset[i] <- base
    else
      dataset[i] <- point2
    if(dataset[i] >base)
      p.up.temp2 <<- p.up2+p.interval2*(base-dataset[i])/(limit.max2-dataset[i])
    else
      p.up.temp2 <<- p.up2+p.interval2*(base-dataset[i])/(dataset[i]-limit.min2)
   # p.up.temp2 <<- p.up.temp2 + runif(1, randomchange*-1, randomchange)
    if(p.up.temp2<1 && p.up.temp2>0){
      p.up2 <<- p.up.temp2
      p.down2 <<- 1-p.up2
    } 
  }
  else{
    pH.int <- pH.interval*runif(1, 0.1, 1)
    rdm <- floor(runif(1, -1*p.down, p.up))
    if(rdm == 0)
      rdm <- 1
    point <- dataset[i-1]+pH.int*rdm
    if(point>limit.max || point<limit.min)
      dataset[i] <- dataset[i-1]
    else
      dataset[i] <- point
    if(dataset[i] >base)
      p.up.temp <<- p.up+p.interval*(base-dataset[i])/(limit.max-dataset[i])
    else
      p.up.temp <<- p.up+p.interval*(base-dataset[i])/(dataset[i]-limit.min)
    p.up.temp <<- p.up.temp + runif(1, randomchange*-1, randomchange)
    if(p.up.temp<1 && p.up.temp>0){
      p.up <<- p.up.temp
      p.down <<- 1-p.up
    } 
  }
}

plot(dataset, type="l", ylab="pH(-)",xlab="",lwd=2, cex.lab=1,col="black",cex=1)