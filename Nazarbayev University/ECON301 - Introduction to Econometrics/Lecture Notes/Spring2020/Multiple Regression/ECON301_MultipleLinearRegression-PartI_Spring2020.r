rm(list=ls())

library(wooldridge)
library(tidyverse)

#load the data set attend

data(attend)
?attend

model1<-lm(final~missed,data=attend)
summary(model1)

model2<-lm(final~missed+priGPA,data=attend)
summary(model2)

auxreg2<-lm(priGPA~missed,data=attend)
summary(auxreg2)

data(wage2)
?wage2

summary(select(wage2,educ,IQ))

model3<-lm(lwage~educ,data=wage2)
summary(model3)

model4<-lm(lwage~educ+IQ,data=wage2)
summary(model4)


cor(wage2$educ,wage2$IQ)

head(select(wage2,educ,IQ),20)
auxreg1<-lm(IQ~educ,data=wage2)
summary(auxreg1)

