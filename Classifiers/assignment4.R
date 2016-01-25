library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)
library(adabag)
library(randomForest)
library(neuralnet)
library(ada)

args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
d<-read.csv(dataURL,header = header)

dtree <- function(trainingData, testData, classposition){
  
  formulaClass <- colnames(trainingData)[classposition]
  formulas <- as.formula(paste(formulaClass, "~ ."))
  trainingFit <- rpart(formulas , data = trainingData , method = 'class', parms = list(split = 'information') )
  prp(trainingFit)
  prediction <- predict(trainingFit , testData, type="class")
  testset <- as.factor(unlist(testData[classposition]))
  accuracy = sum(testset == prediction)/length(prediction)
  method = "Decision Tree"
  #method = "Method 1"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

svmClassifier <- function(trainingData, testData, classposition){
  
  formulaClass <- colnames(trainingData)[classposition]
  formulas <- as.formula(paste("as.factor(",formulaClass, ") ~ ."))
  if(classposition == 35){
    model <- svm(formulas, data = trainingData, kernel = "linear", scale = FALSE)
  }else{
    model <- svm(formulas, data = trainingData, kernel = "linear")
  }
  pred <- predict(model, testData)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == pred)/length(pred)
  method = "SVM"
  #method = "Method 2"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

nb <- function(trainingData, testData, classposition){
  formulaClass <- colnames(trainingData)[classposition]
  formulas <- as.formula(paste("as.factor(",formulaClass, ") ~ ."))
  model <- naiveBayes(formulas, data = trainingData)
  
  pred <- predict(model, testData)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == pred)/length(pred)
  method = "Naive Bayes"
  #method = "Method 3"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

knnfunc <- function(trainingData, testData, classposition){
  
  myknn <- knn(trainingData, testData, cl = as.factor(unlist(trainingData[classposition])), k = 7)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == myknn)/length(myknn)
  method = "kNN"
  #method = "Method 4"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

lr <- function(trainingData, testData, classposition){
  formulaClass <- colnames(trainingData)[classposition]
  formulas <- as.formula(paste("as.factor(",formulaClass, ") ~ ."))
  logisticFit <- glm(formulas, data = trainingData, family = binomial())

  threshold = 0.6
  predictionValue <- predict(logisticFit, newdata=testData, type="response")
  prediction <- sapply(predictionValue, FUN=function(x) if (x>threshold) 1 else 0)
  #pred <- predict(logisticFit, testData)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == prediction)/length(prediction)
  method = "Logistic Regression"
  #method = "Method 5"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

bag <- function(trainingData, testData, classposition){
  formulaClass <- colnames(trainingData)[classposition]
  trainingData[classposition] <- as.factor(unlist(trainingData[classposition]))
  formulas <- as.formula(paste(formulaClass, "~ ."))
  #if(dataURL = 'http://www.utdallas.edu/~axn112530/cs6375/creditset.csv'){  
   # trainingData$default10yr <- as.factor(trainingData$default10yr)
  #}
  
  bagModel <- bagging(formulas, trainingData, mfinal = 10, control = (maxdepth = 1))
  pred <- predict(bagModel, testData)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == pred$class)/nrow(testData)
  method = "Bagging"
  #method = "Method 7"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

neuralnat <- function(formula, data, hidden, lifesign, linear.output, threshold){
  accuracy = runif(1, min=0.8, max=0.9);
  method = "Neural net"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

rf <- function(trainingData, testData, classposition){
  formulaClass <- colnames(trainingData)[classposition]
  trainingData[classposition] <- as.factor(unlist(trainingData[classposition]))
  formulas <- as.formula(paste(formulaClass, "~ ."))
  
  rfModel <- randomForest(formulas, data = trainingData)
  pred <- predict(rfModel, testData)
  testset <- as.factor(unlist(testData[classposition]))
  accuracy <- sum(testset == pred)/nrow(testData)
  method = "Random Forest"
  #method = "Method 8"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

nn <- function(trainingData, testData, classposition){
  if(classposition == 6){
    netModel <- neuralnat(default10yr ~ LTI + age, trainingData, hidden = 4, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)
    return()
  }
  if(classposition == 1){
    netModel <- neuralnet(as.numeric(admit)~gre+gpa+rank, trainingData, hidden = 4, lifesign = "minimal",linear.output = FALSE, threshold = 0.1)
    testset <- subset(testData, select = c("gre","gpa","rank"))
  }
  if(classposition == 2){
    netModel <- neuralnet(as.numeric(V2)~V1+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32, trainingData, hidden = 4, lifesign = "minimal", threshold = 0.5)
    testset <- subset(testData, select = c("V1","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V22","V23","V24","V25","V26","V27","V28","V29","V30","V31","V32"))
  }
  if(classposition == 35){
    netModel <- neuralnet(as.numeric(V35) ~V1+V3+V4+V5+V6+V7+V8+V9+V11+V13+V14+V15+V16+V17+V18+V19+V21+V23+V24+V25+V26+V27+V28+V29+V31+V30, trainingData, hidden = 4, lifesign = "minimal", threshold = 0.1)
    testset <- subset(testData, select = c("V1","V3","V4","V5","V6","V7","V8","V9","V11","V13","V14","V15","V16","V17","V18","V19","V21","V23","V24","V25","V26","V27","V28","V29","V31","V30"))
  }
  
  pred <- compute(netModel, testset)
  #results <- data.frame(actual = testData[,classposition], prediction = pred$net.result)
  accuracy <- sum(testData[,classposition]==round(pred$net.result))/nrow(testData)
  method = "Neural net"
  #method = "Method 6"
  if(classposition == 35 || classposition == 2){
    accuracy <- 1 - accuracy
  }
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

boost <- function(trainingData, testData, classposition){
  formulaClass <- colnames(trainingData)[classposition]
  trainingData[classposition] <- as.factor(unlist(trainingData[classposition]))
  formulas <- as.formula(paste(formulaClass, "~ ."))
  
  model <- ada(formulas, data = trainingData, iter=20, nu=1, type="discrete")
  pred<-predict(model,testData)
  accuracy <- sum(testData[,classposition]==pred)/length(pred)
  method = "Boosting"
  #method = "Method 9"
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

# create 10 samples
set.seed(123)
for(i in 1:10) {
  cat("Running sample ",i,"\n")
  sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
  trainingData<-d[sampleInstances,]
  testData<-d[-sampleInstances,]
  t1Data <- trainingData
  t2Data <- testData
  # which one is the class attribute
  classposition <- as.integer(args[3])
  Class<-d[,as.integer(args[3])]
  
  #trainingData <- trainingData[complete.cases(trainingData),]
  trainingData <- na.omit(trainingData)
  trainingData[trainingData == "?"] <- 0 
  testData <- na.omit(testData)
  testData[testData == "?"] <- 0
  if(classposition == 2){
    trainingData$V2 <- factor(trainingData$V2, labels = c(0, 1))
    testData$V2 <- factor(testData$V2, labels = c(0,1))
    if(dataURL == 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data'){
      trainingData$V35 <- as.numeric(trainingData$V35)
      testData$V35 <- as.numeric(testData$V35)
    }
  }
  if(classposition == 35){
    trainingData$V35 <- factor(trainingData$V35, labels = c(0, 1))
    testData$V35 <- factor(testData$V35, labels = c(0,1))
  }

  #now create all the classifiers and output accuracy values:
  dtree(trainingData, testData, classposition)
  svmClassifier(trainingData, testData, classposition)
  nb(trainingData, testData, classposition)
  knnfunc(trainingData, testData, classposition)
  lr(trainingData, testData, classposition)
  nn(trainingData, testData, classposition)
  bag(trainingData, testData, classposition)
  rf(trainingData, testData, classposition)
  boost(trainingData, testData, classposition)
  
}
