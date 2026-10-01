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

#Loading the data. This example is based from in example 8.4 from 
#Wooldridge(2016) chapter 8. It models a simple house price equation. 

library(wooldridge)
data(hprice1)

?hprice1



Intercept<-rep(1,88)
lprice<-hprice1$lprice
lground<-hprice1$llotsize
lbuilding<-hprice1$lsqrft
bedrooms<-hprice1$bdrms
colonial<-hprice1$colonial


#I will now create the regressor matrix X
X<-cbind(Intercept, lground, lbuilding, bedrooms, colonial)


mlrhouse<-Fit1(lprice,X,88,4)


printout<-cbind(mlrhouse$beta_hat,mlrhouse$StdError,mlrhouse$tstat,mlrhouse$Pvalue)

colnames(printout)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=7)
printout

data(wage1)
?wage1

lwage<-log(wage1$wage)
Intercept<-rep(1,526)
educ<-wage1$educ
exper<-wage1$exper
tenure<-wage1$tenure
nonwhite<-wage1$nonwhite
female<-wage1$female
married<-wage1$married
numdep<-wage1$numdep



X<-cbind(Intercept,educ,exper,tenure,nonwhite,female,married,numdep)

mlrwageeq<-Fit1(lwage,X,526,7)


printout<-cbind(mlrwageeq$beta_hat,mlrwageeq$StdError,mlrwageeq$tstat,mlrwageeq$Pvalue)

colnames(printout)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout