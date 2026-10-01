##In this exercise you will again perform some simulation experiments to understand the difference
#between the population regression model and the estimated model. You need to use R to simulate
#random numbers from the Uniform and Normal distributions.
#
#(a) Start by generating 500 observations on xi—the explanatory variable—from the uniform distribution
#with range [0, 10]. What are the sample mean and sample standard deviation of the
#xi?
#The R code for generating 500 observations on xi
#is following:

rm(list=ls())
set.seed(1) # Initialize the (default) random-number generator
x <- runif(500, min=0, max=10)
cat("The sample mean of x is ",mean(x),"\n")
cat("The sample standard deviation of x is ",sd(x))

#(b) Randomly generate 500 errors, ui, from the Normal(0,36) distribution. Is the sample average of
#the ui exactly zero? Why or why not? What is the sample standard deviation of the ui?
#The R code for generating 500 errors ui is:

u <- rnorm(500, mean=0, sd=6) # Normal(0,36) distribution
cat("The sample average of u is ",mean(u),"\n")
cat("The sample standard deviation of u is ",sd(u))

#The sample average of ui
#is not exactly zero. This is because the sample mean is not a constant but also a 
#random variable. You are only observing an outcome of that RV.

#(c) Now generate the yi as
#yi = 1 + 2xi + ui;
#that is, the population intercept is one and the population slope is two. use the data to run the
#regression of yi on xi. What are your estimates of the intercept and slope? Are they equal to
#the population values in the above equation? Explain.

#The code to generate the yi and the subsequent regression is

y <- 1+2*x+u
regression_5 <- lm(y~x)
summary(regression_5)

#From the regression results, you can read the estimated values for the intercept and the slope. 
#They are not equal to the population values, but they are close. This is because we 
#are doing a estimation using sample data, and by what we learned in class, the OLS estimators
#are random variables themselves; we are only observing an estimate or outcome of those
#random variables.

#(d) Obtain the OLS residuals (ûi), and verify that sum(ûi)=0  and that sum(xi ûi)=0
#

u_hat <- regression_5$residuals
sum(u_hat)
sum(x*u_hat)


#(e) Compute the same quantities as in the previous part but now use the errors ui instead of the
#residuals. Now what do you conclude?
#

sum(u)
sum(x*u)

#These quantities will not be zero necessarily. These properties are only true for the OLS residuals, 
#but not for the population errors.

#(f) Repeat parts (a), (b) and (c) with a new sample of data, starting with generating the xi. Now
#what do you obtain for the OLS estimators? Why are these different from what you obtained in part (c)?

x <- runif(500, min=0, max=10)
u <- rnorm(500, mean=0, sd=6)
y <- 1+2*x+u
regression_5 <- lm(y~x)
summary(regression_5)

#The estimates will of course be different. We took a new sample from a population with the same distribution. 
#Different samples give different estimates. 
