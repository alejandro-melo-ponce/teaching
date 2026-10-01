rm(list=ls())
#Extra Problem Set

betahat<-c(1028.097247,19.297062,-2.194828,-45.091450,-169.812644,62.306357)
names(betahat)<-c("Intercept","hsize","hsizesq","female","black","female x black")

Omega<-rbind(   c(40.386516,   -20.199194,     2.335095,    -9.031893,   -11.536296,     8.210525),  
                c(-20.199194,    14.545411,    -1.900289,     0.078404,     1.261833,     0.068763),  
                c(2.335095,    -1.900289,     0.274387,    -0.000038,    -0.077895,     0.031187),  
                c(-9.031893,     0.078404,    -0.000038,    17.863001,     8.820194,   -17.816754),  
                c(-11.536296,     1.261833,    -0.077895,     8.820194,   237.863505,  -237.367652),  
                c(8.210525,     0.068763,     0.031187,   -17.816754,  -237.367652,   372.702161) )
colnames(Omega)<-c("Intercept","hsize","hsizesq","female","black","female x black")
rownames(Omega)<-c("Intercept","hsize","hsizesq","female","black","female x black")

betahat
Omega

#function for Testing Hypothesis on Linear Combinations
WaldLinear<-function(Omega,lambda,betahat,ThetaHip){
  Num<-sum(lambda*betahat)-ThetaHip
  Var<-t(lambda)%*%Omega%*%lambda
  StdErr<-sqrt(Var)
  tstat<-Num/StdErr
  Pvalue<-2-2*pnorm(abs(tstat),0,1)
  out<-list(StdErr,tstat,Pvalue)
  return(out)
}
#End of function WaldLinear

#Function for Testing a General Linear Hypothesis
WaldG<-function(Omega,HG,betahat,ThetaHip,df){
  Num<-HG%*%betahat-ThetaHip
  Vari<-solve(HG%*%Omega%*%t(HG))
  Wcalc<-t(Num)%*%Vari%*%Num
  Pvalue<-1-pchisq(Wcalc,df)
  out<-list(Wcalc,Pvalue)
  return(out)
}
#End of function WaldG
