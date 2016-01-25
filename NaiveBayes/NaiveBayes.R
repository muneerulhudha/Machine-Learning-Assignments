library(caret)
library(e1071)

dataset <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")

nb <- function(){
  accuracy <- 0
  
  primaPart <- createDataPartition(dataset$V9, p = .9,list = FALSE,times = 1)
  trainingData <- dataset[primaPart,]
  testData <- dataset[-primaPart,]

  model <- naiveBayes(as.factor(V9) ~ ., data = trainingData)
  pred <- predict(model, testData)
  accuracy <- sum(testData$V9 == pred)/length(pred)
  
  return(accuracy)
}

acc <- c()
for(i in 1:10){
  acc <- c(acc,nb())
}
print("Accuracy in each iteration: ")
print(acc)
overallAcc <- mean(acc)

print("Overall Accuracy: ")
print(overallAcc)