walmart=read.csv("D:/datasets/walmart-train.csv")
walmart

na=complete.cases(walmart)
nv=which(na[]==FALSE)
walmart_num=data.frame(walmart[-nv,])


walmart_num=data.frame(walmart)

for(i in 1:ncol(walmart_num))
{
  walmart_num[[i]]=as.numeric(walmart_num[[i]])
}

#elbow curve
x=c()
for(i in 1:38) {
  walmart_cluster=kmeans(walmart_num[,2:7],i,nstart=50)
  x[i]=walmart_cluster$withinss
}
walmart_cluster1=data.frame(c(1:38),x)


g=ggplot(walmart_cluster1,aes(1:38,x))+geom_line()+geom_point()
ggplotly(g)



# # -----------------------------------------------------------------------

sample=sort(sample(nrow(walmart),nrow(walmart)*.7))

walmart_train=walmart[sample,]
walmart_test=walmart[-sample,]

model=naiveBayes(TripType ~.,data=walmart_train)
model

pred=predict(model,walmart_test[,-1])
pred

confmat=table(pred,walmart_test$TripType)
confmat

accuracy=sum(diag(confmat))/sum(confmat)
accuracy


accuracy=sum(diag(confmat))/sum(confmat)
accuracy


# # -----------------------------------------------------------------------


tree.modelll=tree(TripType~.,data=walmart_train)
tree.modelll

model_prediction=predict(tree.model,my_data.test)
model_prediction






# .' ----------------------------------------------------------------------

library(e1071)
library(dplyr)
train=read.csv("D:/datasets/walmart-train.csv")
test=read.csv("D:/datasets/walmart-test.csv")
train=na.omit(train)
for(i in 1:ncol(train)){
  train[,i]=as.numeric(train[,i])
}

for(i in 1:ncol(test)){
  test[,i]=as.numeric(test[,i])
}
train=train%>%mutate(weekend=if_else(Weekday=="Saturday"|Weekday== "Sunday",1,0))

train=train%>%mutate(weekday=if_else(Weekday!="Saturday"&Weekday!= "Sunday",1,0))

test=test%>%mutate(weekend=if_else(Weekday=="Saturday"|Weekday== "Sunday",1,0))

test=test%>%mutate(weekday=if_else(Weekday!="Saturday"&Weekday!= "Sunday",1,0))

#naive bayes
train$TripType=as.factor(train$TripType)
model=naiveBayes(TripType~VisitNumber+DepartmentDescription+FinelineNumber+weekend+weekday,data=train)
pred=predict(model,test[,c(1,5,6,7,8)],type="raw")
pred
pred=as.data.frame(pred)
visitno=test[,1]
pred=cbind(pred,visitno)
write.csv(pred,"D:/ml/kagle2/result1.csv")

















 








