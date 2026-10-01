#Wage example from Wooldridge (example 2.4 page 30)
#Simple Linear Regression


#Section 1.
#Input of data

library(wooldridge)

#Input data
data(wage1)

#What does wage1 contain?
?wage1

#The first few lines
head(wage1)

#Creation of dependent and explanatory variables:

wage<-wage1$wage
education<-wage1$educ
gender<-wage1$female




#Section 2

#Plots and Estimation of Simple Linear Regression
#Scatter plot of wage vs education 
plot(education,wage)
slr1<-lm(wage~education)
summary(slr1)

plot(gender,wage)

slr2<-lm(wage~gender)
summary(slr2)

#Plot of Fitted Model 
x<-seq(from=0, to=18,by=1)
wage_plot<- -0.90485+0.54136*x
points(x,wage_plot,type="b",col="red",pch=16)


#Section 3
#Computation of residuals
wage_hat<--0.90485+0.54136*education
residual<-wage-wage_hat
#Plot of residuals vs education and histogram of residuals
plot(education,residual)
hist(residual,breaks=20)
#Estimation of Conditional variance
n<-length(wage)
sigma2_hat<-sum(residual*residual)/(n-2)
sigma2_hat
sigma_hat<-sqrt(sigma2_hat)
sigma_hat
