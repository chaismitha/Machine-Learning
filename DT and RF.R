setwd("D:/datasets")
install.packages("tree")
library(tree)
tree_data<-read.csv(file="weather_data.csv",header=TRUE)
tree_data
tree.model=tree(Play~.,data=tree_data)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

# weather even data --------------------------------------------------------

even_data<-read.csv(file="weather_data_even.csv",header=TRUE)
even_data
tree.model=tree(Play~.,data=even_data)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

# for specific features ---------------------------------------------------

tree.model=tree(Play~Outlook+Temperature+Humidity,data=even_data)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

# weather odd data --------------------------------------------------------

odd_data<-read.csv(file="weather_data_odd.csv",header=TRUE)
odd_data
tree.model=tree(Play~.,data=odd_data)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)



#  ------------------------------------------------------------------------

tree_data<-read.csv(file="weather_data.csv",header=TRUE)
tree_data
#sort(sample(nrow(data),nrow(data)*.7)) to take sample

my_data.train<-tree_data[1:10,]
my_data.test<-tree_data[-(1:10),]

tree.model=tree(Play~.,data=my_data.train)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)


# to testing the train data -----------------------------------------------

model_prediction=predict(tree.model,my_data.test)
model_prediction

maxidx=function(arr){
  return(which(arr==max(arr)))}
idx=apply(model_prediction,c(1),maxidx)
modelprediction=c('No','Yes')[idx]
confmat=table(modelprediction,my_data.test$Play)
confmat
accuracy=sum(diag(confmat))/sum(confmat)
accuracy


#  ------------------------------------------------------------------------


even_data<-read.csv(file="weather_data_even.csv",header=TRUE)
even_data

my_data.train<-even_data[1:35,]
my_data.test<-even_data[-(1:35),]

tree.model=tree(Play~.,data=my_data.train)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

model_prediction=predict(tree.model,my_data.test)
model_prediction

maxidx=function(arr){
  return(which(arr==max(arr)))}
idx=apply(model_prediction,c(1),maxidx)
modelprediction=c('No','Yes')[idx]
confmat=table(modelprediction,my_data.test$Play)
confmat
accuracy=sum(diag(confmat))/sum(confmat)
accuracy


# sample picking ----------------------------------------------------------

even_data<-read.csv(file="weather_data_even.csv",header=TRUE)
even_data

df=sort(sample(nrow(even_data),nrow(even_data)*.9))
my_data.train<-even_data[df,]
my_data.test<-even_data[-df,]

tree.model=tree(Play~.,data=my_data.train)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

model_prediction=predict(tree.model,my_data.test)
model_prediction

maxidx=function(arr){
  return(which(arr==max(arr)))}
idx=apply(model_prediction,c(1),maxidx)
modelprediction=c('No','Yes')[idx]
confmat=table(modelprediction,my_data.test$Play)
confmat
accuracy=sum(diag(confmat))/sum(confmat)
accuracy


#  ------------------------------------------------------------------------

tree.model=tree(Play~Temperature+Wind+Outlook,data=my_data.train)
tree.model
summary(tree.model)
plot(tree.model)
text(tree.model)

model_prediction=predict(tree.model,my_data.test)
model_prediction

maxidx=function(arr){
  return(which(arr==max(arr)))}
idx=apply(model_prediction,c(1),maxidx)
modelprediction=c('No','Yes')[idx]
confmat=table(modelprediction,my_data.test$Play)
confmat
accuracy=sum(diag(confmat))/sum(confmat)
accuracy



#  RANDOM FOREST ----------------------------------------------------------

tree_data=read.csv(file="weather_data_rf.csv",header=TRUE)
tree_data
sample_items=sample.int(n=nrow(tree_data),size=floor(.90*nrow(tree_data)),replace=F)
sample_items

tree_data_train=tree_data[sample_items,]
tree_data_train

tree_data_test=tree_data[-sample_items,]
tree_data_test

#tree model 1

tree.model1=tree(Play~., data=tree_data_train)
tree.model1
summary(tree.model1)

#tree model2

tree.model2=tree(Play~Temperature+Humidity+Wind, data=tree_data_train)
tree.model2
summary(tree.model2)

#tree model3

tree.model3=tree(Play~Humidity+Outlook, data=tree_data_train)
tree.model3
summary(tree.model3)

for(i in 1:nrow(tree_data_test))
{
  model_prediction1=predict(tree.model1,tree_data_test[i,])
  print(model_prediction1)
  model_prediction2=predict(tree.model2,tree_data_test[i,])
  print(model_prediction2)
  model_prediction3=predict(tree.model3,tree_data_test[i,])
  print(model_prediction3)
  readline(prompt = "Press[enter] to continue")
}



# K-NN -------------------------------------------------------------------

pc_data<- read.csv(file="prostate_cancer.csv")
pc_data
head(pc_data)
summary(pc_data)
pc_train_data<-pc_data[1:70,2:9]
pc_test_data<-pc_data[-(1:70),2:9]

pc_train_data
pc_test_data

pc_train_label<-pc_data[1:70,1]
pc_test_label<-pc_data[-(1:70),1]

pc_train_label
pc_test_label

install.packages("class")
library(class)

k=4

pc_pred_label=knn(train=pc_train_data,test=pc_test_data,cl=pc_train_label,k)
pc_pred_label

confmat=table(pc_test_label,pc_pred_label)
confmat

accuracy=sum(diag(confmat))/sum(confmat)
accuracy

k=7

pc_pred_label=knn(train=pc_train_data,test=pc_test_data,cl=pc_train_label,k)
pc_pred_label

confmat=table(pc_test_label,pc_pred_label)
confmat

accuracy=sum(diag(confmat))/sum(confmat)
accuracy







