##logestic regression model on the train data set:
## I splitted the data into to subsets (1 subset with 70% of the data and the other with the remaining 30%)
##Made a new column named 'defaulters' that shows 0 for people who payed their full loans which they had the value 0 in the loss column 
##and 1 for people who had greater than 0 in the loss column,, then I labeled them as non defaulters and defaultrs.

data1<-read.csv('train_v3.csv')

### Imputation ###
preProcModel <- preProcess(data1, method = "medianImpute")
data<-predict(preProcModel, data1)

is.na(data)

write.csv(data,"train_data_withoutNA.csv")

data<-read.csv('train_data_withoutNA.csv')
data<-data[-763]
head(data)

data$defaulters<-ifelse(data$loss>0,1,0)
data$defaulters<-ordered(data$defaulters, levels = c(0,1),labels = c("Non defaulters","defaulters"))
str(data$defaulters)

set.seed(100)
ind<-sample(2,nrow(data),
            replace=T,
            prob=c(.7,.3))
training<-data[ind==1,]
testing<-data[ind==2,]


model1<-glm(defaulters ~.,family = "binomial",training)
summary(model1)

##model for classification
library (xgboost)
library(gbm)
library(caret)
custom <-trainControl(method='repeatedcv', number = 10, repeats = 1, verboseIter = TRUE)
model2<-train(defaulters ~., training, method='gbm',trControl = custom)
model_pred<-predict(model2, newdata =as.matrix(testing[-763]))


##After running the first model, it turned out we only have 12 significant columns,
##So I counducted another model that only deals with the 12 columns

model2<-glm(defaulters ~f199+f189+f169+f152+f81+f80+f71+f61+f47+f41+f5+X,family = "binomial",training)
summary(model2)

### This is only the calculation process ( never mind it )defaulters = -3.407e+00 +-2.431e-01  (f199)+ 6.503e-02(f189) + -3.524e-01(f169) 

##Prediction process, I used 50% probability of whos likley to defualt but it gave me all approved because it's a high probability
pred<-predict(model2,newdata = testing, type="response")

attributes(pred)
summary(pred)
pred<-ifelse(pred>0.5,1,0)
pred

## This to see error percentages 
pred<-ordered(pred,levels=c(0,1),labels=c("Non defaulters","defaulters"))
View(testing[,760:764])
table1=table(actual=testing$defaulters,predicted=pred)

## This is to see how accurate the model is
##17441/19273 = 90% accuracy for our model
sum(diag(table1))/sum(table1)

##NOW lets Apply the model on the original test file

testdata<-read.csv('test_scenario1_2 (1).csv')

pred<-predict(model2,newdata = testdata, type="response")
pred
summary(pred)

## if a person is predicted to have a prob of defulting greater than 20% then we won't approve giving them a loan
## I made the probability lesser than before when I used the 50% so some people can be rejected.
pred<-ifelse(pred>0.2,1,0)
pred
pred<-ordered(pred,levels=c(0,1),labels=c("Non defaulters","defaulters"))

## checking on the data (not important)
testdata$predicted<-pred
View(testdata[,760:765])
View(subset(testdata,predicted=="Non defaulters")[760:765])
loansgiving<-subset(testdata,predicted=="Non defaulters")

##Calculating total amount of loans we're giving , and it's smaller than 1.4B
## The total of approved loans amounts is 1,031,788,764 it's whitin budget 
sum(loansgiving$requested_loan)

##new column with approved 1 or not approved 0
testdata$approved <- ifelse(testdata$predicted=="Non defaulters",1,0)
View(testdata[,760:766])

#number of customers we're approving it's the same rows number as the summed total of loans approved, so it's all good
sum(testdata$approved,na.rm = TRUE)
nrow(loansgiving)


