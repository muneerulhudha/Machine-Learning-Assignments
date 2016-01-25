library(caret)
library(class)

dataset <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")

knnfunc <- function(kvalue = 1){
  acc <- c() 
  for(i in 1:10){
    accuracy <- 0
    primaPart <- createDataPartition(dataset$V9, p = .9,list = FALSE,times = 1)
    trainingData <- dataset[primaPart,]
    testData <- dataset[-primaPart,]
  
    myknn <- knn(trainingData, testData, cl = trainingData$V9, k = kvalue)
    accuracy <- sum(testData$V9 == myknn)/length(myknn)
    
    acc <- c(acc, accuracy)
  }
  print(paste("Accuracy for kvalue ", kvalue) )
  print(mean(acc))
}

knnfunc(3)
knnfunc(5)
knnfunc(7)
knnfunc(9)
knnfunc(11)