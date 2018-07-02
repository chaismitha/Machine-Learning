install.packages("e1071")
library(e1071)
setwd("D:/datasets")
HouseVotes84=read.csv(file="HouseVotes84.csv",header=TRUE)
HouseVotes84
set.seed(111)
sample=sample.int(n=nrow(HouseVotes84),size=floor(.7*nrow(HouseVotes84)),replace=F)
sample
HouseVotes84_train=HouseVotes84[sample,]
HouseVotes84_train
HouseVotes84_test=HouseVotes84[-sample,]
HouseVotes84_test

model=naiveBayes(Class~.,data=HouseVotes84_train)
model

pred=predict(model,HouseVotes84_test[,-1])
pred

confmat=table(pred,HouseVotes84_test$Class)
confmat

accuracy=sum(diag(confmat))/sum(confmat)
accuracy


accuracy=sum(diag(confmat))/sum(confmat)
accuracy







library(rpart)
library(rpart.plot)
treeFit<- rpart(salary~.,data=data.train,method = 'class')
print(treeFit)





