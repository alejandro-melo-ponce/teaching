#Lecture 3, part 5
#Example 3.3
#Simulation of Misspecification error
#when a relevant variable is excluded in the model. 
#variables
#Savings=Dependent variable
#Income=Explanatory variable 1
#Expenditure=Explanatory variable 2

#Data generation, n=6, i.e., 6 observations. 

#We will create vectors of length 6 containing the observations

Income<-c(10,10,11,10.5,10.5,10)
Expenditure<-c(2,2,3,3,1,1)
constant<-c(1,1,1,1,1,1)
X<-cbind(constant,Income,Expenditure)
X
#True Ceteris paribus effects
beta<-c(4,2,-4)
beta

#True conditional standard deviation
sigma<-0.5

#Generation of the true unobserved errors of the model 
eps<-rnorm(6,0,sigma)
eps

#Generation of the observations
Savings<-X%*%beta+eps
Savings
#Regression including all relevant variables 
betag<-(solve(t(X)%*%X))%*%t(X)%*%Savings
betag

#Regression excluding expenditure
X1<-cbind(constant,Income)
X1
#LSE with Income only
betag1<-(solve(t(X1)%*%X1))%*%t(X1)%*%Savings
betag1

#Scatter diagram and fitted model, Income only
yhat<-X1%*%betag1
plot(Income,Savings)
points(Income,yhat,type="l")

#Computation of Bias
X_1<-Income
X_2<-Expenditure
avg1<-mean(X_1)
avg2<-mean(X_2)
X1dev<-X_1-avg1
X2dev<-X_2-avg2
Sx1x1<-sum(X1dev*X1dev)
Sx1x2<-sum(X1dev*X2dev)
Bias<-beta[3]*Sx1x2/Sx1x1
Bias

#Expected value 
Esp<-beta[2]+Bias
Esp
