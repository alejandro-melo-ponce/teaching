#Clean the work space
rm(list=ls())

#load some packages: wooldridge for the datasets, tidyverse and dplyr
#if it is the first time, you will need to install them first, as I showed you in class, before you 
#load them with the commands below


#This package includes the teaching datasets from the book. I will constantly use this one.
library(wooldridge)
#These two packages are for handling and tidying data. They are incredibly useful, in particular 
#if you want to dwelve more into data science and big data (highly valuable in today's labor market)
library(tidyverse)
library(dplyr)

#load HTV into the workspace
data("htv")
#what is contained HTV
?htv
#A computer description of each of the variables
str(htv)
#descriptive statistics for all the variables, try to subset and summarize just some of them
#You can see how I do this below. 
summary(htv)
#I want to identify each row by row number, I will need this latter, the next command 
#does precisely that: it creates a new column with the row numbers and saves it to 
#htv_tbl
htv_tbl<-as_tibble(rownames_to_column(htv))
#the next command creates a frequency table. Note the use of the function mutate to create new 
#variables for the table, the cumulative total, the percent and the cumulative percent. 
#The command is grouping the data by education level, and then is counting the number with that education
#level, and then creating the cummulative total, the percent and the cummulative percent.
htv_tbl %>%
  group_by(educ) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)),
         percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)),3))
#Running the regression in the first example and saving it to model 1
model1<-lm(wage~educ,data=htv_tbl)
#A summary of the regression output contained in model 1.
summary(model1)
#I create a new vector with new values which I am gonna feed to the model, i.e., I will use the 
#OLS regression line to predict the expected wage at education levels 6, 8 and 10.
neweduc<-data.frame(educ=c(6,8,10))
#The function predict takes model 1 and uses the education levels in neweduc to predict new values. 
predict(model1,neweduc)
#I want to print here the only observations with an education level of 6 years. 
print(select(filter(htv_tbl,educ==6),c(rowname,wage)))

#This is one way to obtain the sum of squared residuals SSR and the Explained summ of Squares SSE
anova(model1)
#The row corresponding to educ is the SSE since we only have one explanatory 
#variable, the second row corresponds to the SSR. 
anova(model1)[,2]

#I am going to calculate the R squared of the model by hand. First I need the total sum of squares.
#We can do this in two ways: sum what is on the aNOVA table. 

SST1=sum( anova(model1)[,2])

#OR compute this by hand

SST2=sum((htv$wage-mean(htv$wage))^2)

#Notice that the result is exactly the same. 

#I compute the R2
R2model<-anova(model1)[1,2]/SST1

#Notice is the same as in the summary table of model 1 we saw before. 
print(R2model)

#Example 2: share of campaign expenditures by democrat vs percentage of vote to democrat in the US

data(vote1)
?vote1
summary(select(vote1,voteA,shareA))
model2<-lm(voteA~shareA,data=vote1)
summary(model2)

#Example 3: return on equity vs CEO salary

data("ceosal1")
?ceosal1
summary(select(ceosal1,salary,roe))
#salary is in thousands of dollars, roe is in percentage points. See the slides.

model3<-lm(salary~roe,data=ceosal1)
summary(model3)

#Here I create a new variable, roedec, which is in decimals. I save it to a new data frame, ceosal1
ceosal1<-mutate(ceosal1,roedec=roe/100)
model4<-lm(salary~roedec,data=ceosal1)
summary(model4)

#Examples about logarithmic specifications, semielasticities and constant elasticities. 

model5<-lm(lwage~educ,data=htv_tbl)
summary(model5)

model6<-lm(lsalary~lsales,data=ceosal1)
summary(model6)


