#Implementing Stacking using Caret on Iris dataset
#https://www.datatechnotes.com/2019/04/classification-example-with-stacking-in.html
#https://www.pluralsight.com/guides/ensemble-modeling-with-r

library(caret)
library(caretEnsemble)

ir = iris[iris$Species != "setosa",]
ir$Species= factor(ir$Species)
ir
indexes = createDataPartition(ir$Species,p=.90,list=FALSE)

train=ir[indexes,]
test = ir[-indexes,]

method=c("gbm","rpart","rf")
tc= trainControl(method="repeatedcv",number = 5,
                 repeats = 3,classProbs = TRUE)

install.packages('e1071', dependencies=TRUE)
models =caretList(Species~.,data=train,trControl=tc,methodList=method)

output= resamples(models)
summary(output)

dotPlot(output)
stack= caretStack(models,method="glm",trControl=tc)

summary(stack)
pred=predict(stack,test[,1:4])
cm =confusionMatrix(test$Species,pred)
print(cm)

