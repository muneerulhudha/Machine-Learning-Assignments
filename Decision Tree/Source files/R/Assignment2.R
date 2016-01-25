library(rpart)
library(rpart.plot)
args <- commandArgs(TRUE)

trainingData1 <- read.csv(file=args[1])
trainingData2 <- read.csv(file=args[2])
trainingData <- rbind(trainingData1, trainingData2)
trainingData <- lapply(trainingData, factor)
testData <- read.csv(file=args[3])
testData <- lapply(testData, factor)


trainingDataFit <- rpart(Class ~ . , data = trainingData , method = 'class', parms = list(split = 'information') )
prp(trainingDataFit)
plotcp(trainingDataFit)

prunedfit <- prune(trainingDataFit, cp = 0.013462)
prp(prunedfit)

prediction <- predict(trainingDataFit , testData, type="class")
predictionAccuracyBeforePruning = sum(testData$Class == prediction)/length(prediction)

predictionAfterPruning <- predict(prunedfit , testData, type="class")
predictionAccuracyAfterPruning = sum(testData$Class == predictionAfterPruning)/length(predictionAfterPruning)

print("Prediction accuracy before pruning: ")
print(predictionAccuracyBeforePruning)
print("Prediction accuracy after pruning: ")
print(predictionAccuracyAfterPruning)
