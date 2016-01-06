#quiz 2 answers 

#problem 2
library(AppliedPredictiveModeling)
data(concrete)
librarinsy(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(x=training$Superplasticizer)
hist(x = log(training$Superplasticizer))

#problem 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

summary(training)
IL <-grep('^IL_', names(training))

preproc <- preProcess(training[,IL],method = 'pca', thresh = 0.9)

#problem 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#creating IL train / test data
IL[[length(IL)+1]] <- 1
IL_frame_train <- training[,IL]
IL_frame_test <- testing[,IL]
#build two models one normal one with pca
modelFit1 <- train(diagnosis ~ ., method='glm',data=IL_frame_train)
modelFit2 <- train(diagnosis ~ ., method='glm',preProcess='pca',data=IL_frame_train)

predict1<-predict(modelFit1,newdata=IL_frame_test)
predict2<-predict(modelFit2,newdata=IL_frame_test)

confusionMatrix(predict1,IL_frame_test$diagnosis)
confusionMatrix(predict2,IL_frame_test$diagnosis)
