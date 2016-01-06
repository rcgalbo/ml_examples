#plotting predictors for wages data
library(ISLR); library(ggplot2); library(caret);
#data from ISLR package
data(Wage)
summary(Wage)

#partition data
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training); dim(testing)

#create a features plot on entire data set
featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage,
            plot='pairs')

#using ggplot2 with added colour
qplot(age,wage,colour=jobclass, data = training)
#using ggplot2 with regresison smoothers
qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method = 'lm', formula = y`x)


#creating a plot with plotly
library(plotly)
plot_ly(training, x = age, y = wage, mode = 'markers', color = age, size = wage)
