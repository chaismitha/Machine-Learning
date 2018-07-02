
movies=read.csv("D:/datasets/train_v2.csv")


train_data=movies[,-c(1)]
library(arules)
library(recommenderlab)
library(reshape2)
library(knitr)
ratingmatrix=acast(train_data,user ~ movie)

class(ratingmatrix)

ratingmatrix=as.matrix(ratingmatrix)
ratingmatrix=as(ratingmatrix, "realratingmatrix")





data("MovieLense")

MovieLense

sample_movielense=sample.int(nrow(MovieLense),0.9*(nrow(MovieLense)))

train_movielense=MovieLense[sample_movielense]
test_movielense=MovieLense[-sample_movielense]


#user based collebration filtering
recom_model=Recommender(train_movielense,method="UBCF")

predict_ml=predict(recom_model,test_movielense,n=3)

as(predict_ml,"list")
as(test_movielense,"list")



# - -----------------------------------------------------------------------

movie=read.csv("D:/datasets/Movie_Data1.csv")


ratingmatrix=acast(movie,Reg.Ids ~ Movie.Name)

class(ratingmatrix)

ratingmatrix=as.matrix(ratingmatrix)

ratingmatrix<-as(ratingmatrix, "realRatingMatrix")

#sample_movie=sample.int(nrow(ratingmatrix),0.9*(nrow(ratingmatrix)))

#train_movie=ratingmatrix[sample_movie]
#test_movie=ratingmatrix[-sample_movie]


recom_movie=Recommender((train_movie),method="UBCF")

predict=predict(recom_movie,test_movie,n=4)

as(predict,"list")
as(test_movie,"list")


# index -------------------------------------------------------------------


unique(movie$Reg.Ids)->index

test=ratingmatrix[c(3)]
train=ratingmatrix[-c(3)]

recom=Recommender(train,method="UBCF")

pre=predict(recom,test,n=4)

as(pre,"list")
as(test,"list")



# # -----------------------------------------------------------------------


train_data=read.csv("D:/datasets/Movie_Data1.csv")

head(train_data)

library("reshape2")

ratingmatrix<-acast(train_data,Reg.Ids~Movie.Name)

class(ratingmatrix)
#convert it as a matrix
ratingmatrix<-as.matrix(ratingmatrix)
library("arules")
library("recommenderlab")

#Convert it into realratingmatrix data structure
#Realratingmatrix 
ratingmatrix<-as(ratingmatrix,"realRatingMatrix")

#Sample taken randomly 90% and 10%

#sample_movielense=sample.int(nrow(ratingmatrix),0.9*(nrow(ratingmatrix)))
#train_movielense=ratingmatrix[sample_movielense]
#test_movielense=ratingmatrix[-sample_movielense]

#Sample taken only regid=80 based on index
unique(train_data$Reg.Ids)->index
train_movielense=ratingmatrix[-c(3),]
test_movielense=ratingmatrix[c(3),]
recom_model_ML=Recommender(train_movielense,method="SVD") # user based collabrative filtering

predit_ML=predict(recom_model_ML,test_movielense,n=3)# using model predict test data for top 3

as(predit_ML,"list")
as(test_movielense,"list")



# ----------------------------------------------------------------------

data("MovieLense")

MovieLense

scheme=evaluationScheme(MovieLense,method="split",train=.9,given=3,goodRating=4)

scheme

algorithms<-list(
  
     "random items"=list(name="RANDOM"),
      "popular items"=list(name="POPULAR"),
      "user-based CF" = list(name="UBCF"),
     "svd"=list(name="SVD")
)

results=evaluate(scheme,algorithms,n=c(1,3,5,10,15,20,25))

plot(results,annotate=1:4,legend="topleft")

#plot(results,"prec/rec",annotate=3)












