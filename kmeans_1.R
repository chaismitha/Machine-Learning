wines <- read.csv(file = "D:/Data Science 2017/Manipal Academy of Data Science/MGADS TERM2/1 Machine Learning/Daily work/7th day/wines_velse.csv")
set.seed(41)
library(ggplot2)
wines_cluster <- kmeans(wines[,2:14],9,nstart = 20)
wines_cluster
wines_cluster1 <- data.frame(wines[,2:14],wines_cluster$cluster)
plot(wines_cluster1[,2:14],col=wines_cluster$wines_cluster1.cluster)

ggplot(wines_cluster1,aes(x=Alcohol,y=Malic.acid,size=Ash,color=as.factor(wines_cluster.cluster),shape=as.factor(wines_cluster.cluster))) + geom_point()


x= vector()
for (i in 1:150) {
 wines_cluster=kmeans(wines[,2:14],i,nstart = 20)
 x[i]=wines_cluster$tot.withinss
 i=i+1
}

print(x)
k=1:149
plot(k,x)
