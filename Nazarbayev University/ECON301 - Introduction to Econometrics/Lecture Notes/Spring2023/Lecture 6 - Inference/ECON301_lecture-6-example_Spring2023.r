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
#What this dataset contains?
?hprice1

#The data has 88 observations on several variables. 

#We are going to estimate the following the best linear predictor of the log 
# of the selling price
#as a function of  the
#log of the square feet of land, the log of the square feet of the building, 
#the number of bedrooms and a dummy variable
#that indicates whether the house is colonial style or other. 

#We start by creating the variables:

Intercept<-rep(1,88)
lprice<-hprice1$lprice
lground<-hprice1$llotsize
lbuilding<-hprice1$lsqrft
bedrooms<-hprice1$bdrms
colonial<-hprice1$colonial

#Before estimating the model, let's look at some graphs:

plot(lground,lprice)
plot(lbuilding,lprice)
plot(bedrooms,lprice)

#I will now create the regressor matrix X
X<-cbind(Intercept, lground, lbuilding, bedrooms, colonial)
X
#4 here is the number of explanatory variables without the intercept
mlrhouse<-Fit1(lprice,X,88,4)

mlrhouse

mlrhouse_R<-lm(lprice~lground+lbuilding+bedrooms+colonial)

summary(mlrhouse_R)

mlrhouse$beta_hat
mlrhouse$Omega




printout<-cbind(mlrhouse$beta_hat,mlrhouse$StdError,mlrhouse$tstat,mlrhouse$Pvalue)

colnames(printout)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=7)
printout

#Residual Box and Whiskers plot: is there evidence of outliers?
b<-boxplot(mlrhouse$eps_hat)
quantile(mlrhouse$eps_hat)




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



#Example 6.2 from Lecture 6, part 4 
lambda<-c(0,-1,1,0,0)

#Is the elasticity of building area greater than the 
#elasticity of ground area?

TestM1<-WaldLinear(mlrhouse$Omega,lambda,mlrhouse$beta_hat,0)
TestM1[[1]]
TestM1[[2]]
TestM1[[3]]

quantile(lground)
quantile(lbuilding)
quantile(bedrooms)
quantile(colonial)

lambda2<-c(1,9.057567,7.708266,4,1)
EstimateLprice<-sum(lambda2*mlrhouse$beta_hat)
EstimateLprice
TestM2<-WaldLinear(mlrhouse$Omega,lambda2,mlrhouse$beta_hat,0)
TestM2[[1]]
EstimateLprice-TestM2[[1]]*1.95
EstimateLprice+TestM2[[1]]*1.95

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

#Testing Model Significance

k=4
HS<-matrix(0,k,k+1)
for(j in 1:k){ HS[j,j+1]=1}
HS

THip<-c(0,0,0,0)

TestSigM1<-WaldG(mlrhouse$Omega,HS,mlrhouse$beta_hat,THip,k)

TestSigM1[[1]]
TestSigM1[[2]]
