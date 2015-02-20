##Quiz 3

#Q1

# Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
  library(AppliedPredictiveModeling)
 data(segmentationOriginal)
 library(caret)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
# 3. In the final model what would be the final model prediction for cases with the following variable values:
#   a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

  set.seed(125)
  inTrain = createDataPartition(y=segmentationOriginal$Case, list=F)
  training = subset(segmentationOriginal, Case=="Train"  )
  testing =  subset(segmentationOriginal, Case=="Test"  )
  
  modFit<- train(Class ~ ., data=training, method="rpart")
print(modFit$finalModel)
plot(modFit$finalModel, uniform=T)
  text(modFit$finalModel, use.n=T, all=T, cex=.9)
install.packages("rattle")
  library(rattle)
fancyRpartPlot(modFit$finalModel)
  
  
  #Q3
  install.packages("pgmm")
  library(pgmm)
  data(olive)
  olive = olive[,-1]
  
  modFit<- train(Area ~ ., data=olive, method="rpart")
  print(modFit$finalModel)
  
  newdata = as.data.frame(t(colMeans(olive)))
  pred<- predict(modFit, newdata)
  
  
#Q4
  install.packages("ElemStatLearn")
  library(ElemStatLearn)
  data(SAheart)
  set.seed(8484)
  train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
  trainSA = SAheart[train,]
  testSA = SAheart[-train,]
  set.seed(13234)
modFit<- train(chd ~  tobacco +	ldl	+	typea	+ obesity	+ alcohol + age, data=trainSA, method="glm", family="binomial")
  missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
  
  
  missClass(trainSA$chd, predict(modFit , newdata = trainSA))
  missClass(testSA$chd, predict(modFit , newdata = testSA))
  
  #Q5
  
  library(ElemStatLearn)
  data(vowel.train)
  data(vowel.test) 
  vowel.test$y<- as.factor(vowel.test$y)
  vowel.train$y<- as.factor(vowel.train$y)
  set.seed(33833)
  
  modFit<- train(y ~ ., data=vowel.train, method="rf", importance = FALSE)
  modFit
  varImp(modFit)
  a <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
  
  