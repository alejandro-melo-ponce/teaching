library(wooldridge)
data(wage1)
?wage1

model1lec5<-lm(formula=lwage~educ, data=wage1)
summary(model1lec5)

model2lec5<-lm(formula=lwage~educ+exper+tenure+nonwhite+female
               +married+numdep,data=wage1)
summary(model2lec5)
