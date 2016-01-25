dataset <- read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE, sep = ",")

hist(dataset$V1, breaks = 17, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "No. of times pregnant", border = "red", main = paste("Histogram of No. of times pregnant"))
barplot(dataset$V1, plot = TRUE, ylab = "No. of times pregnant", xlab = "Instances")

hist(dataset$V2, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Plasma glucose content", border = "red", main = paste("Histogram of Plasma glucose content"))
barplot(dataset$V2, plot = TRUE, ylab = "Plasma glucose content", xlab = "Instances")

hist(dataset$V3, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Diastolic blood pressure", border = "red", main = paste("Histogram of Diastolic blood pressure"))
barplot(dataset$V3, plot = TRUE, ylab = "Diastolic Blood pressure", xlab = "Instances")

hist(dataset$V4, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Triceps skin fold thickness", border = "red", main = paste("Histogram of Triceps skin fold thickness"))
barplot(dataset$V4, plot = TRUE, ylab = "Triceps skin fold thickness", xlab = "Instances")

hist(dataset$V5, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "2-Hour serum insulin", border = "red", main = paste("Histogram of 2-Hour serum insulin"))
barplot(dataset$V5, plot = TRUE, ylab = "2-Hour serum insulin", xlab = "Instances")

hist(dataset$V6, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Body mass index", border = "red", main = paste("Histogram of Body mass index"))
barplot(dataset$V6, plot = TRUE, ylab = "Body mass index", xlab = "Instances")

hist(dataset$V7, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Diabetes pedigree function", border = "red", main = paste("Histogram of Diabetes pedigree function"))
barplot(dataset$V7, plot = TRUE, ylab = "Diabetes pedigree function", xlab = "Instances")

hist(dataset$V8, axes = TRUE, plot = TRUE, labels = TRUE, xlab = "Age", border = "red", main = paste("Histogram of Age"))
barplot(dataset$V8, plot = TRUE, ylab = "Age", xlab = "Instances")

cor(dataset$V1, dataset$V9)
cor(dataset$V2, dataset$V9)
cor(dataset$V3, dataset$V9)
cor(dataset$V4, dataset$V9)
cor(dataset$V5, dataset$V9)
cor(dataset$V6, dataset$V9)
cor(dataset$V7, dataset$V9)
cor(dataset$V8, dataset$V9)

maxCor = 0;
for(i in 1:8){
  for(j in 1:8){
    if(i != j){
      correlation <- cor(dataset[i], dataset[j])
      if(correlation > maxCor)
        maxCor <- correlation
    }
  }
}

maxCor



