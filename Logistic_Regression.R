

claimants <- read.csv(file.choose()) # Choose the claimants Data set
sum(is.na(claimants))
claimants <- na.omit(claimants) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(claimants)
colnames(claimants)
claimants <- claimants[,-1] # Removing the first column which is is an Index
# GLM function use sigmoid curve to produce desirable results  
# The output of sigmoid function lies in between 0-1
model <- glm(ATTORNEY~.,data=claimants,family = "binomial")
summary(model)
# Confusion matrix table 
prob <- predict(model,claimants,type="response")
prob
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62
##ROC
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
