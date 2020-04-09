data("iris")
library(caret)
library(C50)


inTraininglocal <- createDataPartition(iris$Species,p=.70,list =F)
training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]

#Model Building
model <-C5.0(training$Species~.,data=training,trails=20)


#generate model summary
summary(model)


data("iris")
library(caret)
library(C50)




#############Ensemble Method#############
#Bagging & Boosting
acc <- c()
for(i in 1:100)
{
  print(i)
  
  #Data partition
  inTraininglocal <- createDataPartition(iris$Species,p=.70,list=F)
  training1 <- iris[inTraininglocal,]
  testing <- iris[-inTraininglocal,]
  
  #Model Building
  fittree <- C5.0(training1$Species~.,data=training1,trails=20)
  
  #predicting
  pred <- predict.C5.0(fittree,testing[,-5])
  a <- table(testing$Species,pred)
  
  #accuracy
  acc <- c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
boxplot(acc)


#predict for test dataset
pred <- predict.C5.0(model,testing[,-5])
a <- table(testing$Species,pred)
sum(diag(a))/sum(a) #accuracy: 93% is different due to random sample data

plot(model)

