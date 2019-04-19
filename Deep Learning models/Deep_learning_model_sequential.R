dataset2= read.csv('Invoiced December.csv')
data1 = read.csv('Non-bill.csv')
library(dplyr)
library(magrittr)
library(keras)
library(neuralnet)
b<-colnames(data1)
data_set=select(dataset2,b)
beta=rbind(data_set,data1)
#remove to text columns before mutate
which( colnames(beta)=="Billing.Notes")
which( colnames(beta)=="Call.Text")
beta1=beta[,-c(6,5)]
beta1[,-1] %<>% mutate_if(is.factor,as.numeric)
#1.3 Cleaning the texts
#bag of words model 
#install.packages('tm')
library(tm)
corpus_a = VCorpus(VectorSource(beta$Billing.Notes))
#all words in lower case
library(tm)
corpus_a= tm_map(corpus_a, content_transformer(tolower))
library(tm)
corpus_a<- tm_map(corpus_a, PlainTextDocument)
#removing the numbers from text
corpus_a= tm_map(corpus_a, removeNumbers)
#removing punctuations 
corpus_a= tm_map(corpus_a, removePunctuation)
corpus_a= tm_map(corpus_a, removeWords,stopwords() )
corpus_a= tm_map(corpus_a, stemDocument)
#removing unnecesary / irrelevant words = stop words
#install.packages('SnowballC')
library(SnowballC)
#corpus = tm_map(corpus, stemDocument)
#removing extra spaces = stripWhitespace
corpus_a= tm_map(corpus_a, stripWhitespace)


#Creating the bag of words model - One column for each word 
library(tm)
bag_a = DocumentTermMatrix(corpus_a)
#only considering Most Frequent Words for understanding correlation ,reducing sparsity 
#second input is the proportion of words that are repeated to be kept in the bag
library(tm)
bag_a= removeSparseTerms(bag_a, 0.99)
#sparse matrix into a dataframe
bag_a =(as.matrix(bag_a))





#process call text column
library(tm)
corpus_b = VCorpus(VectorSource(beta$Call.Text))
#all words in lower case
library(tm)
corpus_b= tm_map(corpus_b, content_transformer(tolower))
corpus_b<- tm_map(corpus_b, PlainTextDocument)
#removing the numbers from text
corpus_b= tm_map(corpus_b, removeNumbers)
#removing punctuations 
corpus_b= tm_map(corpus_b, removePunctuation)
#corpus_b=tm_map(corpus_b,removeURL)
#removing unnecesary / irrelevant words = stop words
#install.packages('SnowballC')
library(SnowballC)
corpus_b = tm_map(corpus_b, removeWords,stopwords() )
corpus_b= tm_map(corpus_b, stemDocument)

#removing extra spaces = stripWhitespace
corpus_b= tm_map(corpus_b, stripWhitespace)


#Creating the bag of words model - One column for each word 
library(tm)
bag_b = DocumentTermMatrix(corpus_b)
#only considering Most Frequent Words for understanding correlation ,reducing sparsity 
#second input is the proportion of words that are repeated to be kept in the bag
bag_b= removeSparseTerms(bag_b, 0.99)
#sparse matrix into a dataframe
bag_b =(as.matrix(bag_b))











final_processed_data=cbind(beta1,bag_a,bag_b)
dimnames(final_processed_data)
colnames(final_processed_data)
head(final_processed_data)
str(final_processed_data)
write.csv(final_processed_data,file='final_processed_data.csv')
#***************************((((((((((((((((((((((((Model))))))))))))))))))))))))********************************************************
final<-read.csv('final_processed_data.csv')
unique(final$Invoiced..Y.N.)
final$Invoiced..Y.N. = ifelse(final$Invoiced..Y.N.=='Y',1,0)
new2<-as.matrix(final[,-1])

unique(new2[,1])

#create data partition
library(caTools)
library(keras)
library(neuralnet)
library(magrittr)
set.seed(2019)
ind<-sample(2,nrow(new2), replace = T, prob= c(.8,.2))
training <-new2[ind==1,2:642]
trainingtarget<-new2[ind==1,1]
unique(trainingtarget)
test<-new2[ind==2,2:642]
testtarget<-new2[ind==2,1]
traininglabels = to_categorical(trainingtarget)
testlabels = to_categorical(testtarget)

unique(traininglabels)

#model1
library(keras)
model<-keras_model_sequential() 
model %>% 
  layer_dense(units = 32, activation = "relu",input_shape = c(641))%>% 
  layer_dense(units = 32, activation = "relu")%>% 
  layer_dense(units=2,activation = "sigmoid")
summary(model)

#compile model1
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
#fit model
history<-model %>% 
  fit(training,
      traininglabels,
      epochs=30,
      batch_size = 32,
      validation_split =0.2)
plot(history)

#evaluate model1
model %>% evaluate(training, traininglabels)
pred<-model %>% predict_classes(training)
table(Predicted=pred, Actual=trainingtarget)
prob<-model%>%predict_proba(training)

#evluate on test with model1
model%>% evaluate(test,testlabels)
pred<-model%>%predict_classes(test)
table(Predicted=pred, Actual=testlabels)
nrow(Actual)



##dropout model2
dpt_model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(641)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 2, activation = "sigmoid")
dpt_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

#fit droupout model 
dpt_model_hist <- dpt_model %>% fit(
  training,
  traininglabels,
  epochs = 30,
  batch_size = 512,
  validation_data = list(test, testlabels)
)

##evaluate droupout model 
dpt_model %>% evaluate(training, traininglabels)
pred<-dpt_model %>% predict_classes(training)
table(Predicted=pred, Actual=trainingtarget)
prob<-dpt_model%>%predict_proba(training)


#plot comparison 
library(ggplot2)
library(tidyr)
plot_training_losses <- function(losses) {
  loss_names <- names(losses)
  losses <- as.data.frame(losses)
  losses$epoch <- seq_len(nrow(losses))
  losses %>% 
    gather(model, loss, loss_names[[1]], loss_names[[2]]) %>% 
    ggplot(aes(x = epoch, y = loss, colour = model)) +
    geom_point()
}
plot_training_losses(losses = list(
  model = history$metrics$val_loss,
  dpt_model = dpt_model_hist$metrics$val_loss
))
