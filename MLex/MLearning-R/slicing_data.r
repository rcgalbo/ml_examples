#k-fold example 
library(caret)
library(kernlab)
data("spam")

#use createDataPartition to separate out a training and testing sample
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

#create 10 folds
set.seed(0)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain = FALSE)
sapply(folds, length)

#resampling example
set.seed(0)
folds <- createResample(y=spam$type,times = 10, list = True)
sapply(folds, length)
