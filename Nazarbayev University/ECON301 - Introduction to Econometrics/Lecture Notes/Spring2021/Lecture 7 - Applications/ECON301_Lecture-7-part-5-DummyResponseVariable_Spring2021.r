#Clean your workspace
rm(list=ls())

#Function to Fit the multiple linear regression model 
#with Covariance Matrix consistent under heteroskedasticity

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

library(wooldridge)
data(mroz)
?mroz

n<-753
inlf<-mroz$inlf
Intercept<-rep(1,n)
kidslt6<-mroz$kidslt6
kidsge6<-mroz$kidsge6
age<-mroz$age
educ<-mroz$educ
exper<-mroz$exper
nwifeinc<-mroz$nwifeinc
exper2<-exper*exper

X1<-cbind(Intercept,nwifeinc,educ,exper,age,kidslt6,kidsge6)
k1<-6
M1<-Fit1(inlf,X1,n,k1)

printout1<-cbind(M1$beta_hat,M1$StdError,M1$tstat,M1$Pvalue)

colnames(printout1)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout1

X2<-cbind(Intercept,nwifeinc,educ,exper,exper2,age,kidslt6,kidsge6)
k2<-k1+1
M2<-Fit1(inlf,X2,n,k2)

printout2<-cbind(M2$beta_hat,M2$StdError,M2$tstat,M2$Pvalue)

colnames(printout2)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout2



experience<-seq(0,45,by=1)
labor_participation<-0.5855192249+0.0394923895*experience-0.0005963119*experience*experience

plot(experience,labor_participation,type="l")
