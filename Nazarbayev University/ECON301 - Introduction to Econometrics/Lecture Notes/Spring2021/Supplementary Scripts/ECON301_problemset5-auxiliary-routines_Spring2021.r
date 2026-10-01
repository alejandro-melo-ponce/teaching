#Clean your workspace
rm(list=ls())

#Loading the fitting routines. 

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

#Task 1. 

betahat1<-c(-3.3387546,1.49883206,0.48985174)
betahat1

Omega1<-cbind(c(6.00011581,-1.2605606,0.1121914),c(-1.2605606,0.29138551,-0.0384267),c(0.1121914,-0.0384267,0.010413))
Omega1

#a)

tcalc<-betahat[[2]]/sqrt(Omega1[[2,2]])
tcalc

zcrit<-qnorm(0.05,lower.tail=FALSE)
zcrit
pval<-pnorm(tcalc,lower.tail=FALSE)
pval

#b)
lambda<-c(0,1,-1)
lambda
test1b<-WaldLinear(Omega1,lambda,betahat1,0)
#the standard error
test1b[[1]]
#The test statistic
test1b[[2]]
pval<-pnorm(test1b[[2]],lower.tail=FALSE)
pval

#c

tstat<-(betahat1[[3]]-1)/sqrt(Omega1[[3,3]])
tstat
pval<-pnorm(tstat,lower.tail=FALSE)
pval

#d

tstat<-(betahat1[[3]]-1.2)/sqrt(Omega1[[3,3]])
tstat
pval<-pnorm(tstat,lower.tail=FALSE)
pval

#e)
lambda<-c(0,1,1)
lambda
test1b<-WaldLinear(Omega1,lambda,betahat1,1)
#the standard error
test1b[[1]]
#The test statistic
test1b[[2]]
pval<-pnorm(test1b[[2]],lower.tail=FALSE)
pval

# f)

k=2
HS<-matrix(0,k,k+1)
for(j in 1:k){ HS[j,j+1]=1}
HS

Thetahip<-c(0,0)
Thetahip
globalsig1f<-WaldG(Omega1,HS,betahat1,Thetahip,k)
#the wald statistic
globalsig1f[[1]]
#critical value
chicrit<-qchisq(0.05,k,lower.tail=FALSE)
chicrit
#the pvalue
globalsig1f[[2]]

#Task 2

#In the problem set I do it by hand. Here I will use the Fit1 routine to check
#my manual results. 

Y<-c(8,7.5,2.5,2)
Y
Intercept<-rep(1,4)
X1<-c(5,4,2,1)
X<-cbind(Intercept,X1)
X

task2<-Fit1(Y,X,4,1)
task2$beta_hat
#Notice that hatphi^2 is equal to n x Omega[2,2], that is 
hatphi2<-4*task2$Omega[[2,2]]
hatphi2

#The SE of hatbeta1
SEb<-sqrt(hatphi2/4)
SEb

#Building the 95% CI.
SEb*qnorm(0.05/2,lower.tail=FALSE)

task2$beta_hat[2]-SEb*qnorm(0.05/2,lower.tail=FALSE)
task2$beta_hat[2]+SEb*qnorm(0.05/2,lower.tail=FALSE)

#Critical value for the 1% significance test
zcrit<-qnorm(0.01/2,lower.tail=FALSE)
zcrit

#Task 4

betahat4<-c(84.89061,-1.003587,7.332466)
Omega4<-cbind(c(3.704246,-0.119479,1.670767),c(-0.119479,0.004506,-0.117575),c(1.670767,-0.117575,7.328432))
Omega4

#a)
#The same matrix HS from above is still good.
HS
#Also the hypothesis vector thetahip
Thetahip
#we have two explanatory variables 
k<-2
globalsig4<-WaldG(Omega4,HS,betahat4,Thetahip,k)
#The Wald Statistic
globalsig4[[1]]
#The pvalue
globalsig4[[2]]

#b
#the SE is
SE<-sqrt(Omega4[[2,2]])
SE
#the critical value
zcrit<-qnorm(0.05/2,lower.tail=FALSE)
zcrit
#Building the 95% CI
betahat4[[2]]- zcrit*SE
betahat4[[2]]+ zcrit*SE

#c)
tcalc<-betahat4[[3]]/sqrt(Omega4[[3,3]])
tcalc
pvalue<-pnorm(tcalc,lower.tail=FALSE)
pvalue

#Task 5

Intercept<-rep(1,5)
x1<-c(1,1,1,0,0)
Y<-c(2,3,0,2,3)
X<-cbind(Intercept,x1)

#For the sake of the example, I am  going to do this step by step. Feel free to use 
#the Fit1 routine. 

XtX<-t(X)%*%X
XtX
XtX_inv<-solve(XtX)
XtX_inv
#The LSE vector
betahat5<-XtX_inv%*%t(X)%*%Y
betahat5
epshat5<-Y-X%*%betahat5
epshat5
#the vector of squared residuals: each element is the square of the individual residual
#for observation i.
epshat5sq<-epshat5*epshat5
S<-matrix(0,2,2)
for(i in 1:5){S<-S+((X[i,])%*%t(X[i,]))*epshat5sq[i]}
#Here, S is the sandwiched matrix Xt*D*X from the formula in the lecture (slide 16, lecture 6).
#To compute it, I am using the alternative formula with the sumation in slide 15,
#which is easier to implement numerically. 
S
Omega5<-XtX_inv%*%S%*%XtX_inv
Omega5

#To corrobarate we did a good job above
test<-Fit1(Y,X,5,1)
test$beta_hat
test$Omega
