rm(list=ls())

library(wooldridge)
data(gpa2)

?gpa2

reg1<-lm(sat ~ hsize+hsizesq+female+black+female*black,data=gpa2)
summary(reg1)
library(sandwich) 
library(lmtest)

coeftest(reg1, vcov = vcovHC(reg1, "HC0"))

cov1<-vcovHC(reg1,"HC0")
robust.se1<-sqrt(diag(cov1))

robust.se1


library(stargazer)
stargazer(reg1, se=list(robust.se1))

round(cov1,4)

array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}

cat(array_to_LaTeX(round(cov1,6)))

library(car)
linearHypothesis(reg1,vcov = vcovHC(reg1, "HC0"),"hsize+7*hsizesq")[[2,3]]

cov1
sqrt(cov1[[2,2]]+(7^2)*cov1[[3,3]]+2*7*cov1[[2,3]])


reg1$coefficients[[2]]+7*reg1$coefficients[[3]]-qnorm(0.05/2,lower.tail=FALSE)*sqrt(cov1[[2,2]]+(7^2)*cov1[[3,3]]+2*7*cov1[[2,3]])
reg1$coefficients[[2]]+7*reg1$coefficients[[3]]+qnorm(0.05/2,lower.tail=FALSE)*sqrt(cov1[[2,2]]+(7^2)*cov1[[3,3]]+2*7*cov1[[2,3]])
