##Load Data and libraries
mydata <- read.csv("Churn_Train(1).csv")
library(dplyr)
library(ISLR)
library(corrplot)


##Data Exploration
summary(mydata)
str(mydata)

NA_COL <- colMeans(is.na(mydata))
NA_ROW <- rowMeans(is.na(mydata))

boxplot(mydata)
title("Outliers Analysis")
boxplot(mydata$total_day_minutes,mydata$total_eve_minutes,names = c('total_day_minutes','total_eve_minutes'))
title("Variables Containing outliers")
outliers1 <- subset(mydata,mydata$total_day_minutes > 1500)
out_pct1 <-nrow(outliers1)/nrow(mydata) *100
outliers2 <- subset(mydata,mydata$total_eve_minutes > 700)
out_pct2 <- nrow(outliers2)/nrow(mydata)*100

##correlation plot
cor_data <- mydata[c(-1,-3,-4,-5,-20)]
corrplot(cor(cor_data[,1:15]),method = 'number')
cor_data1 <- mydata[c(1,3,4,5,20)]
hetcor(cor_data1)

##Data Cleaning
mydata$account_length <- abs(mydata$account_length)
mydata$number_vmail_messages <- abs(mydata$number_vmail_messages)

# Imputing NA's
mydata[is.na(mydata$account_length),'account_length'] <-round(mean(mydata$account_length,na.rm = TRUE))
mydata[is.na(mydata$number_vmail_messages),'number_vmail_messages'] <-round(mean(mydata$number_vmail_messages,na.rm = TRUE))
mydata[is.na(mydata$total_day_calls),'total_day_calls'] <-round(mean(mydata$total_day_calls,na.rm = TRUE))
mydata[is.na(mydata$total_day_charge),'total_day_charge'] <-mean(mydata$total_day_charge,na.rm = TRUE)
mydata[is.na(mydata$total_eve_calls),'total_eve_calls'] <-round(mean(mydata$total_eve_calls,na.rm = TRUE))
mydata[is.na(mydata$total_eve_charge),'total_eve_charge'] <-mean(mydata$total_eve_charge,na.rm = TRUE)
mydata[is.na(mydata$total_night_minutes),'total_night_minutes'] <-mean(mydata$total_night_minutes,na.rm = TRUE)
mydata[is.na(mydata$total_night_calls),'total_night_calls'] <-round(mean(mydata$total_night_calls,na.rm = TRUE))
mydata[is.na(mydata$total_night_charge),'total_night_charge'] <-mean(mydata$total_night_charge,na.rm = TRUE)
mydata[is.na(mydata$total_intl_minutes),'total_intl_minutes'] <-mean(mydata$total_intl_minutes,na.rm = TRUE)
mydata[is.na(mydata$total_intl_calls),'total_intl_calls'] <-round(mean(mydata$total_intl_calls,na.rm = TRUE))
mydata[is.na(mydata$total_intl_charge),'total_intl_charge'] <-mean(mydata$total_intl_charge,na.rm = TRUE)
mydata[is.na(mydata$number_customer_service_calls),'number_customer_service_calls'] <-round(mean(mydata$number_customer_service_calls,na.rm = TRUE))

a <- subset(mydata,mydata$total_day_minutes < 1500) 
b <- subset(mydata,mydata$total_eve_minutes < 500) 

mydata[is.na(mydata$total_day_minutes),'total_day_minutes'] <-mean(a$total_day_minutes, na.rm = TRUE)
mydata[is.na(mydata$total_eve_minutes),'total_eve_minutes'] <-mean(b$total_eve_minutes, na.rm = TRUE)

summary(mydata)

##Data Partition
set.seed(1234)
index <- sample(2,nrow(mydata),replace = TRUE, prob = c(0.8,0.2))
train <- mydata[index==1,]
test <- mydata[index==2,]

##Models
model <- glm(churn~.,family ="binomial",data=mydata) ## To find significant variables
summary(model)

# Train Data 
model2 <-glm(churn~ state + international_plan + voice_mail_plan + total_day_charge +
               total_intl_calls + number_customer_service_calls ,data=train, family="binomial")
pred_churn <- predict(model2,data=train,type='response')
pc <- as.data.frame(pred_churn)
head(pred_churn)
head(train$churn)
pred_churn=as.factor(pred_churn > 0.50) # if we set pred_churn >0.15 means yes- it gives more correct values of "yes" though the misclassification error is slightly higher
levels(pred_churn) <-list( no='FALSE', yes='TRUE') #change levels
tab1 <- table(Predicted=pred_churn, True=train$churn)
tab1 ##Confusion Matrix for train data
1-sum(diag(tab1))/sum(tab1) ##misclassification rate 

##Auc for train data
roc(as.numeric(train$churn), as.numeric(pred_churn))
plot(roc(as.numeric(train$churn), as.numeric(pred_churn)), col='red', lwd=3)

# Classification tree for churn
library(rpart)
f <- rpart(train$churn~,method="class", data=train)
plot(f, uniform=TRUE,main="Classification Tree for Churn")
text(f, use.n=TRUE, all=TRUE, cex=.7)

#Test data
pred_churn1 <- predict(model2,newdata=test,type='response')
pred_churn1 = as.factor(pred_churn1>0.15)
levels(pred_churn1) <-list( no='FALSE', yes='TRUE') #change levels
tab2 <- table(Predicted=pred_churn1, True=test$churn)
tab2 # Confusion Matrix for test data
1-sum(diag(tab2))/sum(tab2) ##misclassification rate 

##Auc for test data
roc(as.numeric(test$churn), as.numeric(pred_churn1))
plot(roc(as.numeric(test$churn), as.numeric(pred_churn1)), col='red', lwd=3)

#Customers_To_Predict
will_churn <-predict(model2,newdata=Customers_To_Predict,type='response') ## To find the probabilities of churning
Churn_Prob <- will_churn
will_churn = as.factor(will_churn>0.50) 
levels(will_churn) <-list( no='FALSE', yes='TRUE') 
will_churn = as.data.frame(will_churn) ## will have answer in yes or no
Churn_Prob <- cbind(Churn_Prob,will_churn)

## Graph for relation between a significant variables & churn depending on historical data.
par(mfrow=c(3,2))  
plot(train$churn ~state + international_plan + voice_mail_plan + total_day_charge + total_intl_calls + number_customer_service_calls , data = train, type = "c")

# Another Prediction model that we came across(predicts better than out glm)
library(RWeka)
tree<- J48(churn~.,data=train)
tree
table<-table(train$churn,predict(tree))
table
plot(table)     
plot(train$churn ~., data = train, type = "c")

