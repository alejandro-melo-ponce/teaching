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

n<-88

Intercept<-rep(1,n)
lprice<-hprice1$lprice
lground<-hprice1$llotsize
lbuilding<-hprice1$lsqrft
bedrooms<-hprice1$bdrms
colonial<-hprice1$colonial



#I will now create the regressor matrix for the first Model M1 with the full dataset

X1<-cbind(Intercept, lground, lbuilding, bedrooms, colonial)

#This model has k1=4 explanatory variables
k1<-4

M1<-Fit1(lprice,X1,n,k1)

#printing the result of M1

printout<-cbind(M1$beta_hat,M1$StdError,M1$tstat,M1$Pvalue)

colnames(printout)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=7)
printout

#Residual Box and Whiskers plot: is there evidence of outliers?
boxplot(M1$eps_hat)

#Identification of outliers model M1

for(i in 1:n){if(abs(M1$eps_hat[i])>0.4)print(i)}

#Extracting outliers from estimation procedure: creating auxiliary dummy variables
x24<-rep(0,n)
x76<-rep(0,n)
x81<-rep(0,n)

x24[24]<-1
x76[76]<-1
x81[81]<-1

#Running regression without outliers 24, 76, 81
X2<-cbind(Intercept,lground,lbuilding,bedrooms, colonial,x24,x76,x81)
k2<-7

M2<-Fit1(lprice,X2,n,k2)

printout2<-cbind(M2$beta_hat,M2$StdError,M2$tstat,M2$Pvalue)
colnames(printout2)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=7)
printout2

#Is there still evidence of outliers?
boxplot(M2$eps_hat)

#Identification of outliers in M2
for(i in 1:n){if(abs(M2$eps_hat[i])>0.3)print(i)}

#Extracting outliers from estimation procedure: auxiliary dummy variable
x54<-rep(0,n)
x54[54]<-1

#Running regression without outliers 24,76,82,54
X3<-cbind(Intercept,lground,lbuilding,bedrooms, colonial,x24,x76,x81,x54)
k3<-8
M3<-Fit1(lprice,X3,n,k3)

printout3<-cbind(M3$beta_hat,M3$StdError,M3$tstat,M3$Pvalue)
colnames(printout3)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout3

#are there any outliers still in the model?
boxplot(M3$eps_hat)
