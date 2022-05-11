#Ryan Coslove rmc326
#HW 11 Students Future Earnings

#Renaming the dataset and using lm() to build the model for the entire dataset
library(readr)
data <- read.csv('Earnings_Train2022.csv', sep = ",")
View(data)
library(rpart)
library(rpart.plot)

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


model <- lm(Earnings~., data = data)
summary(model)
train_pred <- predict(model, newdata = data)
errorMSE_complete_data <- mse(data$Earnings, train_pred)
errorMSE_complete_data
#Subsetting the dataset based on Major and Number of Professional Connections

other <- data[data$Major == "Other",]
other$squared <- other$Number_Of_Professional_Connections^2
s = data[data$Major == "STEM",]
h = data[data$Major == "Humanities",]
p = data[data$Major == "Professional",]
v = data[data$Major == "Vocational",]
b = data[data$Major == "Buisness",]

#For Majors
other <- subset(data, Major == 'Other')
split1 <- 0.7*nrow(other)
split1
other_train <- other[1:split1,]
other_train
other_test <- other[split1:nrow(other),]
model1 <- lm(Earnings~Major+Number_Of_Professional_Connections+GPA+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets, data = other_train)
summary(model1)

## Train Evaluation for 'Data101'
train_pred1 <-  predict(model1, newdata = other_train)
errorMSE_other_train <- mse(other_train$Earnings, train_pred1)
errorMSE_other_train

## Test Evaluation for 'Data101'
test_pred1 <- predict(model1, newdata = other_test)
errorMSE_other_test <- mse(other_test$Earnings, test_pred1)
errorMSE_other_test
