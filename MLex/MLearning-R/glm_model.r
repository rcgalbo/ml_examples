#test out a sample spam filter using caret package and kernlab sample data

library(caret)
library(kernlab)

#import spam mail for simple classifier
data("spam")

#use createDataPartition to separate out a training and testing sample
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

#fit model
set.seed(0)
modelFit <- train(type ~., data = training, method='glm')
modelFit
#show the model
modelFit$finalModel

#predict for testing sample
predictions <- predict(modelFit,newdata=testing)

#produce a confusion matrix
confusionMatrix(predictions,testing$type)
