##Q1 



##Q2 - load the following code
## Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
##Color by each of the variables in the data set (you may find the cut2() 
##function in the Hmisc package useful for turning continuous covariates into factors).
##What do you notice in these plots?

library(AppliedPredictiveModeling)
library(Hmisc)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

names <- colnames(concrete)
names <- names[-length(names)]

featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
index <- seq_along(1:nrow(training))


plot(training$CompressiveStrength,  pch=10)
qplot(index,CompressiveStrength, data=training, color=FineAggregate )


##Q3 Make a histogram and confirm the SuperPlasticizer variable is skewed.
## Normally you might use the log transform to try to make the data more symmetric. 
##Why would that be a poor choice for this variable?

q1<-qplot(Superplasticizer, data=training, aes=hist)
q2<-qplot(log(Superplasticizer + 1), data=training, aes=hist)
grid.arrange(q1,q2,ncol=2)
##The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer
dat<-(log(training$Superplasticizer))

dat
##Q4 Find all the predictor variables in the training set that begin with IL. 
##Perform principal components on these variables with the preProcess() 
##function from the caret package. Calculate the number of principal components 
##needed to capture 90% of the variance. How many are there?
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names<- names(training)
nm<- names[grep("^IL", names)]

preproc<- preProcess(training[,nm], method="pca", thresh = 0.9)
trainpc<- predict(preproc, training[,nm])
preproc$rotation



##Q5 Create a training data set consisting of only the predictors with variable names
## beginning with IL and the diagnosis. Build two predictive models, 
##  one using the predictors as they are and one using PCA with principal components 
## explaining 80% of the variance in the predictors. Use method="glm" in the train
## function. What is the accuracy of each method in the test set? 
## Which is more accurate?

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training1 = adData[ inTrain,]
testing1 = adData[-inTrain,]

t<- training1[, c(1, grep("^IL",colnames(training1)))]
modelFit <- train(diagnosis ~ ., method = "glm", data = t)
predictions <- predict(modelFit, newdata = testing1)
d1 <- confusionMatrix(predictions, testing1$diagnosis)
print(d1)

modelFit <- train(training1$diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training1, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing1$diagnosis, predict(modelFit, testing1))
print(C2)
preproc<- preProcess(t[,-1], method="pca", thresh = 0.8)
trainpc<- predict(preproc, t[,-1])
modelfit<- train(t$diagnosis ~., method="glm", data=trainpc)
predictions <- predict(modelFit, newdata = testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)