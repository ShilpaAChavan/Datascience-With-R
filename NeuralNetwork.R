# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
concrete<-read.csv("D:/Shilpa/Datascience/classroom/dataset/concrete.csv")
concrete_norm <- as.data.frame(lapply(concrete, normalize))
# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)
# visualize the network topology
windows()
plot(concrete_model)
## blue lines are called bias, there is one hidden layer , error is 5
## Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)
#cor relation is 80 %
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons

#2 hidden layer with 5neuron & 2 neuron
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden =c(5,2))
# plot the network
windows()
plot(concrete_model2)
# evaluate the results as we did before
# error is reduced to 1.61
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
#correlation is 93% , accuracy is increased and error is reduced.

#Animation online : 
#https://playground.tensorflow.org/#activation=relu&batchSize=10&dataset=circle&regDataset=reg-plane&learningRate=0.03&regularizationRate=0&noise=0&networkShape=4,2,2&seed=0.04416&showTestData=false&discretize=false&percTrainData=50&x=true&y=true&xTimesY=true&xSquared=true&ySquared=true&cosX=false&sinX=false&cosY=false&sinY=false&collectStats=false&problem=classification&initZero=false&hideText=false

