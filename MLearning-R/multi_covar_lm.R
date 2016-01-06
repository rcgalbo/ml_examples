#multi covars reg with caret
library(ISLR);library(ggplot2);library(caret)
data(Wage)
Wage <- subset(Wage,select = -c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7,list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]


featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage,
            plot='pairs')

qplot(age,wage,colour=education,data=training)

modFit <- train(wage ~ age + jobclass + education,
                method = 'lm', data = training)
finMod <- modFit$finalModel
print(modFit)

#plot the residules
plot(finMod,1,pch=19,cex=0.5,col='#00000010')

qplot(finMod$fitted,finMod$residuals,color = race,data=training)

#Create a plot by index
plot(finMod$residuals,pch=19)

pred <- predict(modFit,testing)
qplot(wage, pred, color=year,data=testing)

#using all covars in model
modFitAll <- train(wage~., data=training,method='lm')
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

print(modFitAll)
