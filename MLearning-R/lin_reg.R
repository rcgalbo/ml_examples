#fitting simple regression model using caret

library(caret);data("faithful"); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,];testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col='blue',
     xlab='Waiting',ylab='Duration')

#create a model and summary
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

#add line to plot from model
lines(trainFaith$waiting,lm1$fitted,lwd=3)

#training and testing error
sqrt(sum((lm1$fitted - trainFaith$eruptions)^2))
sqrt(sum((predict(lm1, newdata=testFaith) - testFaith$eruptions)^2))

#prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval='prediction')
ord <-order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions)
matlines(testFaith$waiting[ord],pred1[ord,],type = 'l')

#pred intervals with caret
modFit <- train(eruptions~waiting, data=trainFaith,method='lm')
summary(modFit$finalModel)
