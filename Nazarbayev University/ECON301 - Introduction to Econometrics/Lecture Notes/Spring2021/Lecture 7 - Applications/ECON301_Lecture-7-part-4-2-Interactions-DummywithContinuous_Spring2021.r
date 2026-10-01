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
tenure<-wage1$tenure
nonwhite<-wage1$nonwhite
female<-wage1$female
married<-wage1$married
numdep<-wage1$numdep

n=526
X1<-cbind(Intercept,educ,exper,tenure,nonwhite,female,married,numdep)
k1<-7

M1<-Fit1(lwage,X1,n,k1)

printout1<-cbind(M1$beta_hat,M1$StdError,M1$tstat,M1$Pvalue)

colnames(printout1)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout1

#Now we will add an interaction term between a continuous variable
#and a dichothomous variable and female and education

femeduc<-female*educ

X2<-cbind(Intercept,educ,femeduc,exper,tenure,nonwhite,female,married,numdep)
k2<-k1+1
M2<-Fit1(lwage,X2,n,k2)

printout2<-cbind(M2$beta_hat,M2$StdError,M2$tstat,M2$Pvalue)

colnames(printout2)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout2

#Now we will check for outliers, and iterate the procedure until we don't have 
#anymore of them. 

boxplot(M2$eps_hat)

for(i in 1:n){if(abs(M2$eps_hat[i])>1) print(i)}

x15<-rep(0,n)
x24<-rep(0,n)
x58<-rep(0,n)
x59<-rep(0,n)
x128<-rep(0,n)
x150<-rep(0,n)
x186<-rep(0,n)
x203<-rep(0,n)
x260<-rep(0,n)
x282<-rep(0,n)
x440<-rep(0,n)
x516<-rep(0,n)

x15[15]<-1
x24[24]<-1
x58[58]<-1
x59[59]<-1
x128[128]<-1
x150[150]<-1
x186[186]<-1
x203[203]<-1
x260[260]<-1
x282[282]<-1
x440[440]<-1
x516[516]<-1

X3<-cbind(Intercept,educ,femeduc,exper,tenure,nonwhite,female,married,numdep,
          x15,x24,x58,x59,x128,x150,x186,x203,x260,x282,x440,x516)
k3<-k2+12
M3<-Fit1(lwage,X3,n,k3)

printout3<-cbind(M3$beta_hat,M3$StdError,M3$tstat,M3$Pvalue)

colnames(printout3)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout3

boxplot(M3$eps_hat)

for(i in 1:n){if(abs(M3$eps_hat[i])>1) print(i)}

x172<-rep(0,n)
x522<-rep(0,n)

x172[172]<-1
x522[522]<-1

X4<-cbind(Intercept,educ,femeduc,exper,tenure,nonwhite,female,married,numdep,
          x15,x24,x58,x59,x128,x150,x186,x203,x260,x282,x440,x516,x172,x522)
k4<-k3+2

M4<-Fit1(lwage,X4,n,k4)

printout4<-cbind(M4$beta_hat,M4$StdError,M4$tstat,M4$Pvalue)

colnames(printout4)<-c("beta_hat","StdError","t-stat","P-value")

options(scipen=15)
printout4

boxplot(M4$eps_hat)

#Interaction plot between female and education

education<-seq(0,16,by=1)
log_wage_male<-0.372230550+0.090869571*education
log_wage_female<-(0.372230550+0.035217560)+(0.090869571-0.023780828)*education
plot(education,log_wage_male,type="l",col="orange",lwd=5,xlab="Education",
     ylab="log_wage by gender")
title("log_wage as a function of education by gender")
lines(education,log_wage_female,type="l",col="red", lwd=5)

legend(0,1.5,c("males","females"),
       lwd=c(5,5),col=c("orange","red"))
