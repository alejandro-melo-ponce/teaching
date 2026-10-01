rm(list = ls())

#Central Limit Theorem Poisson Distribution

#Poisson Distribution
mu<-4
sigma<-sqrt(mu)

x<-seq(0,15,1)
fx<-dpois(x,mu)
plot(x,fx)

#Number of Monte Carlo replications

MC<-10000
Prom<-rep(0,MC)
Zn<-rep(0,MC)

#Sample Size
n<-50
#Vector of averages and standardized averages
for(i in 1:MC){
  X<-rpois(n,mu)
  Prom[i]<-mean(X)
  Zn[i]<-(Prom[i]-mu)/(sigma/sqrt(n))
}
hist(Prom,breaks=1000,freq=FALSE,xlim=c(2,6))
title(sub="Poisson")
mtext(n,side=3)
hist(Zn,breaks=1000,freq=FALSE)
mtext(n,side=3)
title(sub="Poisson")