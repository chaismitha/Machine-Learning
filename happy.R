train = read.csv("D:/term-2/kaggle/understanding happiness/Happy_train.csv")
test = read.csv("D:/term-2/kaggle/understanding happiness/Happy_test.csv")

nrow(train)
sum(is.na(train))

library(VIM)
imputation<-aggr(train,col=c('yellow','red'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(train),gap=3,
                      ylab=c("Missing Data","Pattern"))
library(mice)

# Imputing missing values by using CART method

tempdata <- mice(train,m=5,maxit=5,meth='cart',seed =100)
Data<-complete(tempdata,4)

imputation<-aggr(Data,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(completeData),gap=1,
                      ylab=c("Missing Data","Pattern"), plot=TRUE)

library(e1071)
nav=naiveBayes(happy~.,data=completeData)
nav

pred=predict(nav,test,type="class")

nav_happy=data.frame(c(1:2621),pred)

write.csv(nav_happy,file="test_happy.csv",row.names = FALSE)

