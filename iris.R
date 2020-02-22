data("iris")
View(iris)
install.packages("party")

library(caret)
library(party)
sample <- createDataPartition(iris$Species, p = 0.85, list = FALSE)
train <- iris[sample,]
test <- iris[-sample,]

#model building 
attach(train)
model1 <- ctree(Species~., data = train)

summary(model1)
plot(model1)
attributes(model1)

#evaluation

pred <- predict(model1, test[,-5])

pred
table(pred)

a <- table(test$Species, pred)

acc <- sum(diag(a)/sum(a))
acc #95%

#bagging

accuracy <- c()
for(i in 1:100)
{
  print(i)
  sample <- createDataPartition(iris$Species, p = 0.85, list = FALSE)
  train1 <- iris[sample,]
  test <- iris[-sample,]
  
  #model building 
  attach(train1)
  model1 <- ctree(Species~., data = train1)
  
  summary(model1)
  plot(model1)
  attributes(model1)
  
  #evaluation
  
  pred <- predict(model1, test[,-5])
  
  pred
  table(pred)
  
  a <- table(test$Species, pred)
  
  accuracy <- sum(diag(a)/sum(a))
  
}
  
accuracy
summary(accuracy)
attributes(accuracy)
