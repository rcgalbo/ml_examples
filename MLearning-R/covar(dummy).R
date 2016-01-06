#covar creation / feature creation

#level1: from raw data -> something useable
#level2: Trasforming to tidy and nice covariates
#   - importan to only use on test set
#   - later will use on test but not to start



library(ISLR);library(caret);data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p = 0.7, list = FALSE)

train <- Wage[inTrain,]; test <- Wage[-inTrain,]


#Common covars to add, dummy variables

table(train$jobclass)

#create dummy var aka creating levels for regression
dummies <- dummyVars(wage ~ jobclass, data = train)
head(predict(dummies, newdata=train))


#Removing zero covariates
nsv <- nearZeroVar(train, saveMetrics = TRUE)
nsv

#use splines bs to fit higher level vars
#allows for curvy fitting
library(splines)
bsBasis <- bs(train$age,df=3)
head(bsBasis)

#applying bs to an lm
lm1 <- lm(wage ~ bsBasis, data = train)
plot(train$age,train$wage,pch=19,cex=.5)
points(train$age,predict(lm1), col='red',pch=19,ce=.5)


#predict on testset then check vs true values
predict(bsBasis, age=test$age)

#looking at the fit statistics and plot values on test set
plot(test$age, test$wage, pch=19,cex=.5)
points(train$age,predict(lm1), col='red',pch=19,ce=.5)
