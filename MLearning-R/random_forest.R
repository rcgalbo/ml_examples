#Random forests

#1. bootstrap samples
#2. at each split, bootstrap variables
#3. grow multiple trees and vote

#Very accurate but slow and difficult to interpret
#can be prone to overfitting

#build large number of trees, each based on a bootstrap sample
#at each node, a different subset of the variables contributes to the splits

#going down all the trees, averages all predictions together

#loading the data
data("iris")
library(ggplot2);library(caret)
inTrain<- createDataPartition(y=iris$Species,
                              p=0.7,list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

#training the random forest
modFit <- train(Species~., data=training, method = 'rf',prox =TRUE)
modFit

#shows tree
getTree(modFit$finalModel,k = 2)

#class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p<- qplot(Petal.Width, Petal.Length, col = Species, data=training)
p+geom_point(aes(x=Petal.Width,y = Petal.Length,col=Species),size=5,shape=4,data=irisP)


pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, color=predRight, data=testing,main='newdata Predictions')

