library(dplyr)
require(caTools) 
library(e1071)
library(glmnet)
library('caret')

#dataset = read.csv('train_v3.csv')

data = dataset
data['loss'] =data['loss']/100
#df_final = subset(data, loss > 0)

#impute missing
imput = preProcess(data, method ='medianImpute')
df_missing=predict(imput,data)

X=data.matrix(df_missing[,-763])
Y=as.vector(df_missing$loss)
cvfit = cv.glmnet(X, Y)
impvar<-coef(cvfit, s = "lambda.min")
col = impvar[impvar[,1] !=0,0]

column_list = col@Dimnames[[1]][2:92]

df_final = select(df_missing,column_list,'loss')

head(df_final)
set.seed(2019)
split=sample.split(df_final$loss, SplitRatio = 0.80)
train=subset(df_final, split==TRUE)
test=subset(df_final,split==FALSE)

pca = preProcess(x= train[-92], method = 'pca',pcaComp = 20,thresh = 0.95)
train = predict(pca,train)
train_set = train[c(2:21,1)]

test = predict(pca,test)
test_set = test[c(2:21,1)]

control <- trainControl(method='repeatedcv',number=5,repeats=1)
tunegrid <- expand.grid(.mtry=c(1:10))
rf_full <-train(loss ~ . , data = train_set, method = "rf", trControl=control,tuneGrid=tunegrid)
#gbm<-train(loss ~ . , data = train_set, method = "gbm", trControl=control)

ypred_full <- predict(rf_full,newdata=test_set)

plot(rf_full)
rf_full$results
rf_full$bestTune

#Predict on Test
dataset_test = read.csv('test_scenario1_2.csv')

#impute missing
imput_test = preProcess(dataset_test, method ='medianImpute')
df_missing_test=predict(imput_test,dataset_test)

df_final_test = select(df_missing_test,column_list)

pca = preProcess(x= df_final_test, method = 'pca',pcaComp = 20,thresh = 0.95)
train_test = predict(pca,df_final_test)

ypred_test_data <- predict(rf_full,newdata=train_test)

dataset_test['prediction'] <- ypred_test_data

#write.csv(dataset_test,'results_scenario1_2.csv')

#Test scenario3 predication
dataset_test_scenario3 = read.csv('test_scenario3.csv')
imput_test_scenario3 = preProcess(dataset_test_scenario3, method ='medianImpute')
df_missing_test_scenario3=predict(imput_test_scenario3,dataset_test_scenario3)
df_final_test_scenario3 = select(df_missing_test_scenario3,column_list)
#length(names(df_final_test_scenario3))

pca_test_scenario3 = preProcess(x= df_final_test_scenario3, method = 'pca',pcaComp = 20,thresh = 0.95)
train_test_scenario3 = predict(pca,df_final_test_scenario3)

ypred_test_scenario3 <- predict(rf_full,newdata=train_test_scenario3)

dataset_test_scenario3['prediction'] <- ypred_test_scenario3

#write.csv(dataset_test_scenario3,'results_scenario3.csv')

result = data.frame(ypred_test_data,ypred_test_scenario3)

#write.csv(result,'results.csv')
