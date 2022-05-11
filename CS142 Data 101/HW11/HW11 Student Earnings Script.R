#Ryan Coslove rmc326
#HW 11 Students Future Earnings

library(readr)
data <- read.csv('Earnings_Train2022.csv', sep = ",")
View(data)
library(rpart)
library(rpart.plot)

tree <- rpart(Earnings ~ Number_Of_Parking_Tickets+Number_Of_Credits+
                Graduation_Year+Major+Number_Of_Professional_Connections+GPA,
              data=data)
rpart.plot(tree)
pred =  predict(tree, newdata = data)
mean((pred-data$Earnings)^2)


#"Freestyle predictions"
boxplot(data$Earnings~data$Major, data = data, xlab="Major",ylab="Earnings",     #major is something
        main="Boxplot of Major vs Earnings",col=rainbow(7),border="black")
boxplot(data$Earnings~data$GPA, data = data, xlab="GPA",ylab="Earnings", 
        main="Boxplot of GPA vs Earnings",col=rainbow(7),border="black")
boxplot(data$Earnings~data$Number_Of_Parking_Tickets, data = data, xlab="Parking Tickets",ylab="Earnings", 
        main="Boxplot of Parking Tickets vs Earnings",col=rainbow(7),border="black")
boxplot(data$Earnings~data$Number_Of_Professional_Connections, data = data, xlab="Professional Connections",ylab="Earnings", 
        main="Boxplot of Professional Connections vs Earnings",col=rainbow(7),border="black") # a little something (more conn more money)
boxplot(data$Earnings~data$Graduation_Year, data = data, xlab="Graduation Year",ylab="Earnings", 
        main="Boxplot of Graduation Year vs Earnings",col=rainbow(7),border="black")
boxplot(data$Earnings~data$Number_Of_Credits, data = data, xlab="Number_Of_Credits",ylab="Earnings", 
        main="Boxplot of Number_Of_Credits vs Earnings",col=rainbow(7),border="black")
#Earnings is dependent on the major

tapply(data$Earnings,data$Major,mean)
predictCol <- rep(0,nrow(data))
predictCol[data$Major == "Buisness"] = 10002
predictCol[data$Major == "Humanities"] = 10249
predictCol[data$Major == "Professional"] = 11748
predictCol[data$Major == "STEM"] = 9748
predictCol[data$Major == "Vocational"] = 13249
predictCol[data$Major == "Other"] = 5845
mean((predictCol-data$Earnings)^2)
#MSE = 348,680.6


#Rpart 
library(rpart)
library(rpart.plot)
tree <- rpart(Earnings ~Major+Number_Of_Professional_Connections,data=data)
rpart.plot(tree)
# Most important factors are major, # professional connections
pred =  predict(tree, newdata = data)
mean((pred-data$Earnings)^2)
#MSE = 93,603.13

#Random Forest 
#install.packages("randomForest")
library(randomForest)
rf <- randomForest::randomForest(Earnings ~ ., data = data)
pred <-  predict(rf, newdata = data, type="class")
mean((pred-data$Earnings)^2) 
# 17750.78


test<- read.csv('Earnings_Test_students.csv', sep = ",")
decision <- prediction
test$predict <- decision
View(test)

sub <- read.csv('earnings_submission.csv', sep = ",")
sub$Earnings <- decision
write.csv(sub, file = "mysubmission.csv",row.names=FALSE)