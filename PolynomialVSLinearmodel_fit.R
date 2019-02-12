#import the data 
salarydata<- read.csv("Salary_Data.csv")
#the data contains data of 50 start up co along with the annual profit of these companies , we will try to know which of the feature has most impact in the profit prediction .
#check the data type
str(salarydata)
summary(salarydata)
#encode the dataset-no missing or categorical data .
# split the data into train and test  
# * not required as the sample size is very small 
# linear regression 
LRM <- lm(formula = Salary ~ . , data =salarydata)
summary(LRM)
# linear regression model plot 
library(ggplot2)
ggplot(salarydata,aes(y=Salary,x=YearsExperience))+geom_point()+geom_smooth(method="lm")

ggplot()+
  geom_point (aes(x =salarydata$YearsExperience, y=salarydata$Salary),
             colour = 'red') +
  geom_line (aes(x =salarydata$YearsExperience, y= predict(LRM, newdata = salarydata)),
            colour = 'blue') +
  ggtitle('Linear Regression Model')+
  xlab('YearsExperience')+
  ylab('Salary')
  
# linear regression model interactive plot 
library(ggiraph)
library(ggiraphExtra)
library(plyr)
ggPredict(LRM,se=TRUE,interactive=TRUE)
#polynomial regression 
#.1. add new levels of variable in the dataset
salarydata$level2 = salarydata$YearsExperience^2
salarydata$level3 = salarydata$YearsExperience^3
salarydata$level4 = salarydata$YearsExperience^5
PolyRM <- lm(formula = Salary ~ . ,data =salarydata)
summary(PolyRM)
#polynomial regression plot 
ggplot()+
  geom_point (aes(x =salarydata$YearsExperience, y=salarydata$Salary),
              colour = 'red') +
  geom_line (aes(x =salarydata$YearsExperience, y= predict(PolyRM, newdata = salarydata)),
             colour = 'blue') +
  ggtitle('Polynomial Regression Model')+
  xlab('YearsExperience')+
  ylab('Salary')

#predicting a new result with Linear regression model 
l_pred <- predict(LRM , data.frame(YearsExperience = 6.5))
l_pred
#predicting a new result with polynomial regression model 
poly_pred <- predict(PolyRM , data.frame(YearsExperience = 6.5,
                                         level2 = 6.5^2 ,
                                         level3=6.5^3,
                                         level4 =6.5^4 ))
poly_pred
