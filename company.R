setwd("F://R//files")
company <- read.csv("Company_Data.csv")

View(company)
range(company$Sales)
median(company$Sales)

table(company$Sales)
#convert numeric data to categorical data
company$Sales <- cut(company$Sales, breaks = c(0,5,10,17), labels = c("low", "medium", "high"))
View(company)

#replace na values with mean
install.packages("gam")
library(gam)
company <- na.gam.replace(company)
sum(is.na(company))
library(caret)

#partition of data
sample <- createDataPartition(company$Sales, p = 0.75, list = FALSE)

Train <- company[sample,]
Test <- company[-sample,]

View(Test)
library(party)

#model building
model1 <- ctree(Train$Sales~., data = Train )
summary(model1)

#evaluate Testing accuracy
pred <- predict(model1, Test[,-1])
a <- table(pred, Test$Sales)
a
acc <- sum(diag(a))/sum(a)
acc

traing_accuracy <- predict(model1, Train[,-1])
b <- table(traing_accuracy, Train$Sales)

acc1 <- sum(diag(b))/sum(b)
acc1

#bagging

bagging_acc <- c()
for(i in 1:100)
{
  print(i)
  intrain <- createDataPartition(company$Sales, p = 0.75, list = FALSE)
  Training <- company[intrain,]
  Testing <- company[-intrain,]
  
  bag_model <- ctree(Training$Sales~., data = Training)
  bag_pred <- predict(bag_model, Testing[,-1])
  ab <- table(bag_pred, Testing$Sales)
  bagging_acc <- sum(diag(ab))/sum(ab)
}
bagging_acc
