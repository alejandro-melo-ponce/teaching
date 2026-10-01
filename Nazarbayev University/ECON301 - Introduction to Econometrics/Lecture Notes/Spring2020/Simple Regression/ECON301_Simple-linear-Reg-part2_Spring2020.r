rm(list=ls())

library(wooldridge)
library(tidyverse)

#load the data set mathpnl

data(mathpnl)
?mathpnl

#We now filter out schools with missing ptr or ptr=0(which is not possible)
mathpnl_tbl<-mathpnl%>%filter(is.na(ptr)==0 & ptr>0)

#summary of the variables of interest
summary(select(mathpnl_tbl,math4,ptr))

#estimate the model
model1<-lm(math4~ptr,data=mathpnl_tbl)
summary(model1)

#Number of schools with pass rates greater than 66.9589
count(mathpnl_tbl%>%filter(math4>66.9589))

data(wage2)
?wage2

model2<-lm(lwage~educ,data=wage2)
summary(model2)

data(jtrain2)
?jtrain2
