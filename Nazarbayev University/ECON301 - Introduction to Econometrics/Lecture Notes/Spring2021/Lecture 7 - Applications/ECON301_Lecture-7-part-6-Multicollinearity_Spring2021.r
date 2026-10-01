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
data(wage1)
?wage1

lwage<-log(wage1$wage)
Intercept<-rep(1,526)
educ<-wage1$educ
female<-wage1$female
male<-1-female

n=526
X1<-cbind(Intercept,educ,male,female)
k1<-3

#What is wrong with this model? Let us see if we can run it...
M1<-Fit1(lwage,X1,n,k1)

#How to fix it?

#You drop one dummy
X2<-cbind(Intercept,educ,female)
k2<-2
M2<-Fit1(lwage,X2,n,k2)

printout2<-cbind(M2$beta_hat,M2$StdError,M2$tstat,M2$Pvalue)

colnames(printout2)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout2

#You drop the constant term
X3<-cbind(educ,male,female)
#remember the Fit1 routine adds a 1 to the number of explanatory variables because
#it assumes you are using a constant, since in here we are not, that is why we put
#2 in the arguments, but we have 3 explanatory variables. 
M3<-Fit1(lwage,X3,n,2)

printout3<-cbind(M3$beta_hat,M3$StdError,M3$tstat,M3$Pvalue)

colnames(printout3)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout3

