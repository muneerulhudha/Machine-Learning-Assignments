library(caret)
library(e1071)

dataset <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")

svmWithKernel <- function(knel = "radial"){
  acc <- c() 
  for(i in 1:10){
    accuracy <- 0
    primaPart <- createDataPartition(dataset$V9, p = .9,list = FALSE,times = 1)
    trainingData <- dataset[primaPart,]
    testData <- dataset[-primaPart,]
  
    model <- svm(as.factor(V9) ~ ., data = trainingData, kernel = knel)
    pred <- predict(model, testData)
    accuracy <- sum(testData$V9 == pred)/length(pred)
    
    acc <- c(acc, accuracy)
  }
  return(mean(acc))
}

print(svmWithKernel(knel = "linear"))
print(svmWithKernel(knel = "polynomial"))
print(svmWithKernel(knel = "radial"))
print(svmWithKernel(knel = "sigmoid"))