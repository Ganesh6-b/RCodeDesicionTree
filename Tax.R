setwd("F://R//files")
Frauddata <- read.csv("Fraud_check.csv")

View(Frauddata)

colnames(Frauddata)

#convert taxable.income to catagorical variable
range(Frauddata$Taxable.Income)

Frauddata$Taxable.Income <- cut(Frauddata$Taxable.Income, breaks = c(10000,30000,100000), labels = c("Risky", "Good"))

View(Frauddata)

#checking na's
sum(is.na(Frauddata))

#partitions of data
library(caret)
sample <- createDataPartition(Frauddata$Taxable.Income, p = 0.80, list = F)

Train_data <- Frauddata[sample,]
Test_data <- Frauddata[-sample,]

#building a model

library(party)

model1 <- ctree(Train_data$Taxable.Income~., data = Train_data)

#visual
plot(model1)

#evaluation

pred_model1 <- predict(model1, Test_data[,-3])

accuracy <- table(pred_model1, Test_data[,3])

accuracy

accu <- c()
for(i in 1:100)
{
  sample <- createDataPartition(Frauddata$Taxable.Income, p = 0.80, list = F)
  
  Train_data1 <- Frauddata[sample,]
  Test_data <- Frauddata[-sample,]
  
  #building a model
  
  library(party)
  
  model1 <- ctree(Train_data1$Taxable.Income~., data = Train_data1)
  
  #visual
  plot(model1)
  
  #evaluation
  
  pred_model1 <- predict(model1, Test_data[,-3])
  
  accu <- table(pred_model1, Test_data[,3])
  
  accu
}  
