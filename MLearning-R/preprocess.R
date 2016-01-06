library(caret); library(kernlab); data("spam")

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main = '',xlab = 'ave. capital run length')

#large skew in the data
mean(training$capitalAve)
sd(training$capitalAve)

#Standardizing variable
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(training$capitalAve))/sd(training$capitalAve)

mean(trainCapAveS)
sd(trainCapAveS)

#standardizing - test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(training$capitalAve))/sd(training$capitalAve)

mean(testCapAveS)
sd(testCapAveS)

#standardizing using preProcess function

preObj <- preProcess(training[,-58], method = c('center','scale'))
trainCapAve <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

#use the preious preObj to standardize the test data
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


#Standardizing - Box-Cox
#transform continuous data to be normal, using max likelyhood
preObj <- preProcess(training[,-58], method = c('BoxCox'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS);qqnorm(trainCapAveS)


#deaing with N/A values inorder to make nice for pred algs

#create na values
training$capAve <- training$capitalAve
selectNa <- rbinom(dim(training)[1], size = 1, prob = .05) == 1
training$capAve[selectNa] <- NA

#impute and standardize
preObj <- preProcess(training[,-58], method='knnImpute')
capAve <- predict(preObj, training[,-58])$capAve


#standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNa])
quantile((capAve- capAveTruth)[!selectNa])
