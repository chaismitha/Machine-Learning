


#loading the dataset
new_plm_data = read.csv("D:/Tarah/PML_Analysis/new_pml_dataset.csv")

#attach the objects of the data
attach(new_plm_data)

##function to check the outliers percentage
outliersper <- function(x){
  length(which(x >  mean(x) + 3 * sd(x) | x < mean(x) - 3 * sd(x))  ) / length(x)
}


library(dplyr)

##applying outliersper function to each column to know the outlier%
sen <- sapply( new_plm_data, outliersper )
sort(sen)

## subsetting the 3 columns which is having higher Outlier%
df = subset(new_plm_data,select=c(accel_dumbbell_x,gyros_belt_z,magnet_belt_y))

class(df)
head(df)



## creating the ab column with 1 where the columns having outlier values and remaining with 0's
df = df %>% mutate(ab=ifelse(accel_dumbbell_x < -141.5 ,"1",
                             ifelse(accel_dumbbell_x > 102.5 ,"1",
                                    ifelse(gyros_belt_z < -0.47 ,"1" ,
                                           ifelse(gyros_belt_z > 0.25 ,"1" ,  
                                                  ifelse(magnet_belt_y < 537.5 ,"1" ,
                                                         ifelse(magnet_belt_y > 653.5 ,"1","0")))))))     



#### disproportionate sampling  with 100% abnormal data and 10% non abnormal data ###---------------

new_df_1 = subset(df,ab == 1)

## taking sample of 10% of  non abnormal data

samp_data = df %>% filter(ab=="0") %>%sample_frac(.1)

new_df_1 = rbind(new_df_1,samp_data)
#write.csv(new_df_1, "D:/Tarah/PML_Analysis/disprop_data1.csv")
nrow(new_df_1)


set.seed(100)
train_sample=sample(1:nrow(new_df_1),0.80*nrow(new_df_1))
traindata_sample=new_df_1[train_sample,]
dim(traindata_sample)
testdata_sample=new_df_1[-train_sample,]
dim(testdata_sample)
test_ab_sample=testdata_sample[,-4]


library(rpart)

#model 1
sample_model_1=rpart(ab~.,data = traindata_sample,method="class")
sample_pred_1=predict(sample_model_1,test_ab_sample,type = 'class')
model_pred=table(sample_pred_1,testdata_sample$ab)
model_pred

#sample_accuracy=sum(diag(model_pred))/sum(model_pred)
#sample_accuracy
#sensitivity = sensitivity(model_pred)
#specificity(model_pred)
#precision = TP / (TP + FP)
#sensitivity (or) recall = TP / (TP + FN)
#specificity = TN / (TN + FP)
#precision <- posPredValue(model_pred)
#F1 <- (2 * precision * sensitivity) / (precision + sensitivity)

library(caret)
result = confusionMatrix(sample_pred_1,testdata_sample$ab)
result

# F1 Score
result$byClass[7] 


# Model 2
sample_model_2=rpart(ab~.,data = traindata_sample,method="class",control = rpart.control(cp = 0.05))
sample_pred_2=predict(sample_model_2,test_ab_sample,type = 'class')

result_1 = confusionMatrix(sample_pred_2,testdata_sample$ab)
result_1

# F1 Score
result_1$byClass[7] 


#Model 3
sample_model_3=rpart(ab~.,data = traindata_sample,method="class",control = rpart.control(cp = 0.10))
sample_pred_3=predict(sample_model_3,test_ab_sample,type = 'class')

result_2 = confusionMatrix(sample_pred_3,testdata_sample$ab)
result_2

# F1 Score
result_2$byClass[7] 

