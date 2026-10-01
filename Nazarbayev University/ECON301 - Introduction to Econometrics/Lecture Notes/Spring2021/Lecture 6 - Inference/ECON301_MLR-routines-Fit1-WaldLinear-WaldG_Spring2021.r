#Clean your workspace
rm(list=ls())

Fit1<-function(Y,X,n,k){
  k1<-k+1
  XtX<-t(X)%*%X
  XtX_inv<-solve(XtX)
  beta_hat<-XtX_inv%*%t(X)%*%Y
  Y_hat<-X%*%beta_hat
  eps_hat<-Y-Y_hat
  eps2<-eps_hat*eps_hat
  A<-matrix(0,k1,k1)
  for (i in 1:n){
    A<-A+eps2[i]*X[i,]%*%t(X[i,])}
  factor<-n/(n-k1)
  Omega<-XtX_inv%*%A%*%XtX_inv
  #Uncomment the next line if you have a small sample. It is a correction factor
  #Omega<-factor*Omega 
  #Standard Errors, t-statistics,P-values
  StdError<-rep(0,k1)
  tstat<-rep(0,k1)
  Pvalue<-rep(0,k1)
  for (j in 1:k1){
    StdError[j]<-sqrt(Omega[j,j])
    tstat[j]<-beta_hat[j]/StdError[j]
    Pvalue[j]<-2-2*pnorm(abs(tstat[j]),0,1)
  }
  char_vec<-c("beta_hat","Omega","eps_hat","StdErrors","tstat","Pvalue")
  out<-list("beta_hat"=beta_hat,"Omega"=Omega,"eps_hat"=eps_hat,
            "StdError"=StdError,"tstat"=tstat,"Pvalue"=Pvalue)
  return(out)
}
# End of function Fit1

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
