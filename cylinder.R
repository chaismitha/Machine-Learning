
cylinder = read.csv("D:/Tarah/Cylinder_Bands.csv")

dim(cylinder)
head(cylinder)
class(cylinder)
sum(is.na(cylinder))
str(cylinder)
summary(cylinder)
colnames(cylinder)

#sapply(cylinder,mode)


cols <- c("plating.tank","viscosity","humifity","blade.pressure","press.speed","roller.durometer","current.density","chrome.content")

cylinder[cols] <- lapply(cylinder[cols], as.numeric)


cols <- c("anode.space.ratio" , "hardener" , "wax" , "ESA.Amperage" , "ESA.Voltage" , "solvent.pct" , "ink.pct" , "varnish.pct" , "roughness" , "ink.temperature" , "caliper" , "proof.cut" )

cylinder[cols] <- lapply(cylinder[cols], as.integer)


a = select_if(cylinder, is.numeric) 
fun <- function(x){
  
  quantiles <- quantile( x, c(.01, .95 ),na.rm=TRUE)
  x[ x < quantiles[1] ] <- NA
  x[ x > quantiles[2] ] <- NA
  x
}

a = sapply(a,fun)

b = select_if(cylinder, is.factor) 


c = cbind(a,b)

c = replace(c, c == "?", NA)
sum(is.na(c))

numSummary(c)
sapply(c, function(x) sum(is.na(x)))


for(i in 1:ncol(c)){
 c[,i][is.na(c[,i])] <- mean(c[,i], na.rm = TRUE)
}



## convert all columns into lowercases
for(i in 1:ncol(c))
{
  
  levels(c[,i]) <- tolower(levels(c[,i]))
  
}

Mode <- function(x) {
  if(class(x)=="factor"){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
}


for (var in 1:ncol(c)) {
  if (class(c[,var]) %in% c("numeric","interger")) {
    c[is.na(c[,var]),var] <- mean(c[,var])
  } else if (class(c[,var]) %in% c("character", "factor")) {
    c[is.na(c[,var]),var] <- Mode(c[,var])
  }
}

sum(is.na(c))


train=sample(1:nrow(c),0.80*nrow(c))

train_data = c[train,]
test_data= c[-train,]

tree.model=tree(band.type~.,data=train_data)



myfunc=function(df)
{
  print(nrow(df))
  print(ncol(df))
  getmode <- function(v)
  {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  i=1
  while(i<=ncol(df)){
    if("numeric" %in% class(df[,i]))
    {
      a3=mean(na.omit(df[,i]))
      df[is.na(df[,i]),i]=a3
      i=i+1
      
    }else
    {
      a1=na.omit(df[,i])
      a2=getmode(a1)
      df[is.na(df[,i]),i]=a2
      i=i+1
    }
  }
  print(df)
  
}
myfunc(c)

#UNIVARIATE ANALYSIS

#Boxplot
for (col in 1:ncol(c)) {
  if(class(c[,col]) %in% c("numeric","interger")) {
  n=names(c)
  boxplot=boxplot( c[,col],xlab = n[col],ylab='count',col='royalblue2',las=2,main='Barplot')}
}

#Histogram
for (col in 2:ncol(c)) {
  if(class(c[,col]) %in% c("numeric","interger")) {
  n=names(c)
  histogram=hist( c[,col],xlab = n[col],ylab='Frequency',col='green')
  }
}



library(outliers)
i=1
outlier_value=c()
count=ncol(c)
while (i<=count) {
  if(class(c[,col]) %in% c("numeric","interger")) {
  outlier_value=append(outlier_value,outlier(c[i]))
  i=i+1
  }
}
outlier_list=data.frame(outlier_value)
outlier_list


summary=c  %>% group_by(customer,band.type) %>% summarise(total=n()) %>% arrange(-total) %>% head(5)
summary

a = ggplot(summary,aes(x=reorder(customer,-total),y=total))+geom_bar(stat='identity')+facet_wrap(~band.type)


ggplotly(a)


summary=c  %>% group_by(customer,band.type) %>% summarise(total=sum(blade.pressure)) %>% arrange(-total) %>% head(5)
a = ggplot(summary,aes(x=reorder(customer,-total),y=total))+geom_bar(stat='identity')+facet_wrap(~band.type)
ggplotly(a)




summary=c  %>% group_by(customer,cylinder.size) %>% summarise(total=n()) %>% arrange(-total) %>% head(5)
summary

a = ggplot(summary,aes(x=reorder(customer,-total),y=total))+geom_bar(stat='identity')+facet_wrap(~cylinder.size)


ggplotly(a)



summary=c  %>% group_by(customer,cylinder.size,band.type) %>% summarise(total=n()) %>% arrange(-total) %>% head(5)
summary

a = ggplot(summary,aes(x=reorder(customer,-total),y=total,fill=cylinder.size))+geom_point()+facet_wrap(~band.type)


ggplotly(a)



summary=c  %>% group_by(customer,paper.mill.location) %>% summarise(total=n()) %>% arrange(-total) %>% head(20)
summary

a = ggplot(summary,aes(x=reorder(customer,total),y=total,fill=paper.mill.location))+geom_bar(stat="identity")


ggplotly(a)


summary=c  %>% group_by(ink.type,paper.mill.location) %>% summarise(total=n()) %>% arrange(-total) %>% head(10)
summary

a = ggplot(summary,aes(x=reorder(paper.mill.location,total),y=total,fill=ink.type))+geom_bar(stat="identity")


ggplotly(a)

summary = c %>% group_by(customer,paper.mill.location) %>% filter(paper.type=="coated") %>% summarise(n=n()) %>% arrange(-n) %>% head(10)

#ggplot(summary,aes(y=customer,x=paper.mill.location,fill=-n))+geom_tile()

ggplot(summary,aes(y=paper.mill.location,x=customer,fill=-n))+geom_bar(stat="identity")

summary = c %>% group_by(customer,paper.mill.location) %>% filter(paper.type=="uncoated") %>% summarise(n=n()) %>% arrange(-n) %>% head(10)
ggplot(summary,aes(y=paper.mill.location,x=customer,fill=-n))+geom_bar(stat="identity")


#top 10 customers
Top=c  %>% group_by(customer) %>% summarise(total=n()) %>% arrange(-total) %>% head(10)
Top

Top_bands=c  %>% filter(band.type=="band") %>% group_by(customer) %>% summarise(total=n()) %>% arrange(-total) %>% head(10)
Top_bands
Top_nobands=c %>% filter(band.type=="noband")  %>% group_by(customer) %>% summarise(total=n()) %>% arrange(-total) %>% head(10)
Top_nobands



twoplot1 <- ggplot(c, aes(x=hardener, y=wax)) +
  geom_point(shape=1) +geom_smooth(method=lm)+facet_wrap(~band.type)
twoplot1


ggplot(data=c, aes(x=current.density, y=wax))  +
  geom_point(aes(size=as.numeric(customer))) +
  scale_size_continuous(range=c(2,15)) +
  theme(legend.position = "none")+theme_classic()


c %>% ggplot(aes(x= humifity,y= press.speed,color=band.type))+geom_point()

data = as.numeric(c)

corrplot(cor(c))



c %>% ggplot(aes(x=  paper.mill.location,y= wax,color=cylinder.size))+geom_bar(stat="identity")



