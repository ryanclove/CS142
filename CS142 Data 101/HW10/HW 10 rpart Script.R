#Ryan Coslove rmc326
#HW 10 rpart Script

library(readr)
install.packages(rpart)
data <- read.csv('M2022train.csv', sep = ",")
View(data)
library(rpart)
library(rpart.plot)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=10))
rpart.plot(tree) #21%
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=50))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=100))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minsplit=200))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minsplit=400))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minsplit=700))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=25, minssplit=100))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=50, minsplit=50))
rpart.plot(tree) #24%


file <- predict(tree, newdata=data ,type="class")
data$predict <- file
error <- mean(data$Grade != data$predict)
error

#Wpredictions

model1<-rpart(Grade~., data=data[data$Score>50,]);
model2<-rpart(Grade~., data=data[data$Score<=50,]);
model1
model2
pred1 <- predict(model1, newdata=data[data$Score>50,], type="class")
pred2 <- predict(model2, newdata=data[data$Score<=50,], type="class")
myprediction<-data
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>50] <- as.character(pred1)
decision[myprediction$Score<=50] <-as.character(pred2)
myprediction$Grade <-decision
error <- mean(data$Grade!= myprediction$Grade)
error

#

tree <- rpart(Grade ~ Score+Major+Seniority,data=data,control = rpart.control(minbucket=10))
tree

prediction <- predict(tree, data, type="class")
# Lets check the Training Accuracy
mean(data$Grade==prediction) 
# 78.86%


install.packages("devtools") 
devtools::install_github("devanshagr/CrossValidation")

CrossValidation::cross_validate(data, tree, 5, 0.8)

test<- read.csv('M2022test.csv', sep = ",")
decision <- predict(tree, newdata=test ,type="class")
test$predict <- decision
View(test)
sub <- read.csv('M2022submission.csv', sep = ",")
sub$Grade <- decision
write.csv(sub, file = "mysubmission.csv",row.names=FALSE)