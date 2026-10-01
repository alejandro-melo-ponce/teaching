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
exper<-wage1$exper
exper2<-exper*exper

X<-cbind(Intercept,educ,exper,exper2)

mlrwageeq<-Fit1(lwage,X,526,3)

printout<-cbind(mlrwageeq$beta_hat,mlrwageeq$StdError,mlrwageeq$tstat,mlrwageeq$Pvalue)

colnames(printout)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout

#Estimated effect when experience goes from 1 year to 2 years:

estchangeWage1<-100*(0.0410088750+2*(-0.0007135582))
estchangeWage1

#Estimated effect when experience goes from 10 years to 11 years:
estchangeWage10<-100*(0.0410088750+2*(-0.0007135582)*10)
estchangeWage10

#quadratic effect plot. 

quantile(educ)
#Suppose education is at its median of 12.

experience<-seq(0,40,by=1)
log_wage<-0.1279975190 + 0.0903658153*12+0.0410088750*experience -0.0007135582*experience*experience

plot(experience,log_wage,type="l")

turning_point<- -(0.0410088750)/(2*-0.0007135582)
turning_point


quantile(exper)
1-ecdf(exper)(turning_point)
