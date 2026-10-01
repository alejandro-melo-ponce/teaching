rm(list=ls())

library(wooldridge)
library(tidyverse)

#Problem C2

data(ceosal2)
?ceosal2

#i
summary(select(ceosal2,salary,ceoten))

#The average salary is $865 (thousand)
#The average tenure is 7.955 years

#ii

ceosal2%>%group_by(ceoten)%>%summarise(n=n())

#5 CEOs in the sample are in their first year (ceoten=0)

#iii

modelC2<-lm(lsalary~ceoten,data=ceosal2)
summary(modelC2)

#The model predicts that one more year as a CEO increases salary by 0.97%

#Problem C4

data(wage2)
?wage2
#i) The following command prints the averages and standard deviations for wage and IQ. 
wage2%>%summarise_at(c("wage","IQ"),list(mean=mean, sd=sd))

#ii)
model1C4<-lm(wage~IQ,data=wage2)
summary(model1C4)

15*coef(model1C4)[2]

#According to the model, an increase of 15 IQ points increases wage by $124.5/month.

#The model Only explains 9.5% of the variation in wages. 


#iii)
model2C4<-lm(lwage~IQ,data=wage2)
summary(model2C4)

100*coef(model2C4)[2]*15
#Now an increase of 15 IQ points increases wage by 13.21%. 

#Problem C6

data(meap93)
?meap93

#i) It seems plausible that an extra dollar in spending has a larger effect for schools whose 
#expenditures are on the lower-end of the distribution than for schools who are high spenders. 
#At low spending schools, more money can go bo purchasing more books, computers and for hiring 
#better qualified teachers. At high levels of spending, spending would cause little, if any, effect, because
#high-spending schools already have high-quality teachers, nice intrastructure, books, etc. 

#ii) The model is in level-logs, therefore the coefficient on log expend means that a for a one percent increase 
#expenditures, we get an increase in the math10 score of (beta_1/100). Thus, a 10% increase in expenditures 
#increases math10 score by beta_1/10.

#iii)
modelC6<-lm(math10~lexpend,data=meap93)
summary(modelC6)

#iv

(coef(modelC6)[2]/100)*10
#If we increase expenditure by 10 percent, then the predicted math10 score increases by 1.11 percentage points. 
#This is not a huge effect, but is not trivial for low-spending schools, where a 10% increase in spending might
#be a fairly small dollar amount. 

#v

summary(select(meap93,math10,expend))
#Int his data set, the largest value of math10 is 66.70, which is not very close to 100. Thus, in our estimated model,
#the largest predicted value of math10 would be 

coef(modelC6)[1] + coef(modelC6)[2]*log(7419)

#that is, around 30.15375.