rm(list = ls())
#Illustration of WLNN
#Convergence of average to population mean mu
#Consistent estimation of sigma square

#*******Segment 1

N<-500
mu<-7
Xn<-mu+rnorm(N,0,1)
ts.plot(Xn)

#Generation of sequence of averages of Xn
Xprom<-rep(0,N)
Sumi<-0
for(i in 1:N){Sumi<-Sumi+Xn[i]
Xprom[i]<-Sumi/i}
ts.plot(Xprom,xlim=c(1,N),ylim=c(6,8))

#**********Segment 2
Zn<-Xn*Xn
Xprom2<-Xprom*Xprom
#Generation of sequence of averages of Zn
Zprom<-rep(0,N)
Sumi<-0
for (i in 1:N){Sumi<-Sumi+Zn[i]
Zprom[i]<-Sumi/i}
sigma_tilde_2<-Zprom-Xprom2
ts.plot(sigma_tilde_2,xlim=c(1,N))
