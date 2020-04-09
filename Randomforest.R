install.packages("caret",dependencies = TRUE)
install.packages("randomForest")
library(randomForest)

##1000 trees
model<-randomForest(iris$Species~.,data=iris,ntree=1000)

# View the forest results.
print(model)

#Importance of the variable - Lower Gini
print(importance(model))
#Prediction
pred<- predict(model,iris[,-5])
pred #predicted values
table(pred,iris$Species)
#Accurancy : 100%
