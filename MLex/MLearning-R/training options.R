#training options in caret
library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

set.seed(0)
modelFit <- train(type ~., data = training, method = '')
modelFit
