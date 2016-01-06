#preprocess with PCA

#load data
library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M)<- 0
which(M > 0.8,arr.ind = T)

names(spam[c(34,32)])
plot(spam[,34],spam[,32])
#are perfectly correlated
#including both isn't useful

#use pca to chose combination and reduce noise

#flip plot

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

#can see that most of the variability is explained by xaxis or by sum

#problems with genralizing pca
#create the best matrix with the lowest rank
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

#how is summing up the two variables
prComp$rotation


typeColor <- ((spam$type=='spam')*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")


preProc <- preProcess(log10(spam[,-58]+1),method = 'pca',pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2], col=typeColor)

#Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method = 'pca',pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type~., method = 'glm', data=trainPC)


testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

#simple pca included in training set

modelFit <- train(training$type~., method='glm',preProcess='pca',data=training)
confusionMatrix(testing$type, predict(modelFit,testing))