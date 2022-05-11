#Ryan Coslove rmc326
#HW 9 Freestyle Script

barplot(table(M2022train$Grade))


data <- read.csv('M2022train.csv', sep = ",")
boxplot(data$Score~data$Grade,ylab="score", xlab="grade", main="Score vs Grade")
boxplot(data$Score~data$Seniority,ylab="score", xlab="grade", main="Score vs Seniority")
boxplot(data$Score~data$Major,ylab="score", xlab="grade", main="Score vs Major")
range <- tapply(data$Score,data$Grade,range)
range

# Grades
# F = 0 - 88
# D = 1 - 97
# C = 12 - 99
# B = 10 - 98
# A = 30 - 100

mosaicplot(data$Grade~data$Major, main = "Major vs Grade", col=rainbow(7))
mosaicplot(data$Grade~data$Seniority, main = "Seniority vs Grade", col=topo.colors(7))

boxplot(data$Score~data$Major,ylab="score", xlab="grade", main="Score vs Major")
boxplot(data$Score~data$Seniority, ylab="score", xlab="grade", main="Score vs Seniority")

#CS Majors tends to have higher scores, economics lowest
#Seniority does not appear to affect score

barplot(table(data$Grade), main="Frequency of each grade", col=rainbow(7), xlab="grade")

a <- data[data$Score > 30 & data$Score <= 100,] 
barplot(table(a$Grade), main = " Frequency of grades between the score 30 and 100", col=heat.colors(7),xlab="grade") 
# most frequent grade is A

mosaicplot(a$Grade~a$Major, col=heat.colors(7), main="Grade vs Major between score of 30-100", xlab="Grade", ylab="Major")
mosaicplot(a$Grade~a$Seniority, col=heat.colors(7), main="Grade vs Seniortiy between score of 30-100", xlab="Grade", ylab="Seniority")

a2 <- data[data$Score >= 30 & data$Score <= 88,] #B, C, D, F
barplot(table(a2$Grade), main="Frequency of grades between 30-88",col=rainbow(7), xlab="Grade") 
# most frequent is C

mosaicplot(a2$Grade~a2$Major, col=heat.colors(7), main="Grade vs Major between score of 10-88", xlab="Grade", ylab="Major")
mosaicplot(a2$Grade~a2$Seniority, col=heat.colors(7), main="Grade vs Seniortiy between score of 10-88", xlab="Grade", ylab="Seniority")

a3 <- data[data$Score >= 30 & data$Score <= 65,] #B, C, D, F
barplot(table(a3$Grade), main="Frequency of grades between 30-65",col=rainbow(7), xlab="Grade")

mosaicplot(a3$Grade~a3$Major, col=heat.colors(7), main="Grade vs Major between score of 1-88", xlab="Grade", ylab="Major")
mosaicplot(a3$Grade~a3$Seniority, col=heat.colors(7), main="Grade vs Seniortiy between score of 1-88", xlab="Grade", ylab="Seniority")

a4 <- a <- data[data$Score >= 0 & data$Score <= 30,] # A, B, C, D, F
barplot(table(a4$Grade), main = " Frequency of grades between the score 0 and 30", col=heat.colors(7),xlab="grade") 

mosaicplot(a4$Grade~a4$Major, col=heat.colors(7), main="Grade vs Major between score of 0-30", xlab="Grade", ylab="Major")
mosaicplot(a4$Grade~a4$Seniority, col=heat.colors(7), main="Grade vs Seniortiy between score of 0-30", xlab="Grade", ylab="Seniority")

#If score is > 30, seems likely you get an A. Lower is an F.

#Cross Vailidation
train <- data[sample(1:nrow(data)),]
training<-train[1:100,]
testing<-train[101:nrow(train),]
error <- mean(testing$Grade!= testing$predictedGrade)

#Summaries
summary(M2022train[M2022train$Score < 30 & M2022train$Major == "CS",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "CS" & M2022train$Seniority == "Freshman",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "CS" & M2022train$Seniority == "Sophomore",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "CS" & M2022train$Seniority == "Junior",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "CS" & M2022train$Seniority == "Senior",])

summary(M2022train[M2022train$Score < 30 & M2022train$Major == "Economics",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Economics" & M2022train$Seniority == "Freshman",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Economics" & M2022train$Seniority == "Sophomore",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Economics" & M2022train$Seniority == "Junior",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Economics" & M2022train$Seniority == "Senior",])

summary(M2022train[M2022train$Score < 30 & M2022train$Major == "Psychology",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Psychology" & M2022train$Seniority == "Freshman",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Psychology" & M2022train$Seniority == "Sophomore",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Psychology" & M2022train$Seniority == "Junior",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Psychology" & M2022train$Seniority == "Senior",])

summary(M2022train[M2022train$Score < 30 & M2022train$Major == "Statistics",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Statistics" & M2022train$Seniority == "Freshman",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Statistics" & M2022train$Seniority == "Sophomore",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Statistics" & M2022train$Seniority == "Junior",])
summary(M2022train[M2022train$Score > 30 & M2022train$Major == "Statistics" & M2022train$Seniority == "Senior",])

#PREDICTORS

myPrediction <- data
decision <- rep('F',nrow(myPrediction))
# Between a score of 1 and 88 there can be F,D,C,B

decision[data$Score > 40 & data$Score <= 60 & data$Major == "CS"]<- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "CS"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "CS"] <- 'B'
decision[data$Score > 80 & data$aMajor == "CS"] <- 'A'

decision[data$Score > 40 & data$Score <= 60 & data$Major == "Economics"] <- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "Economics"] <- 'C'
decision[data$Score > 70 & data$Score <= 85 & data$Major == "Economics"] <- 'B'
decision[data$Score > 85 & data$Major == "Economics"] <- 'A'

decision[data$Score > 50 & data$Score <= 60 & data$Major == "Psychology"] <- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "Psychology"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "Psychology"] <- 'B'
decision[data$Score > 80 & data$Major == "Psychology"] <- 'A'

decision[data$Score > 40 & data$Score <= 55 & data$Major == "Statistics"] <- 'D'
decision[data$Score > 55 & data$Score <= 70 & data$Major == "Statistics"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "Statistics"] <- 'B'
decision[data$Score > 80 & data$Major == "Statistics"] <- 'A'

myPrediction$Grade <- decision
error <- mean(data$Grade != myPrediction$Grade)


#Submission Data

submission <- read.csv('M2022test.csv', sep = ",")
decision <- rep('F',nrow(submission))

decision[data$Score > 40 & data$Score <= 60 & data$Major == "CS"]<- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "CS"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "CS"] <- 'B'
decision[data$Score > 80 & data$aMajor == "CS"] <- 'A'

decision[data$Score > 40 & data$Score <= 60 & data$Major == "Economics"] <- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "Economics"] <- 'C'
decision[data$Score > 70 & data$Score <= 85 & data$Major == "Economics"] <- 'B'
decision[data$Score > 85 & data$Major == "Economics"] <- 'A'

decision[data$Score > 50 & data$Score <= 60 & data$Major == "Psychology"] <- 'D'
decision[data$Score > 60 & data$Score <= 70 & data$Major == "Psychology"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "Psychology"] <- 'B'
decision[data$Score > 80 & data$Major == "Psychology"] <- 'A'

decision[data$Score > 40 & data$Score <= 55 & data$Major == "Statistics"] <- 'D'
decision[data$Score > 55 & data$Score <= 70 & data$Major == "Statistics"] <- 'C'
decision[data$Score > 70 & data$Score <= 80 & data$Major == "Statistics"] <- 'B'
decision[data$Score > 80 & data$Major == "Statistics"] <- 'A'

file <- read.csv('M2022submission.csv', sep = ",")
file$Grade <- decision
write.csv(file, file = "submission.csv",row.names=FALSE)

