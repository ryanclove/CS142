#Ryan Coslove rmc326

moody <- read.csv("~/Senior Year/Spring 2022/CS142 Data 101/moody2022_new.csv")
table(moody$GRADE)
boxplot(moody$SCORE ~ moody$GRADE, xlab = "Grade", ylab = "Score", 
        main = "Score vs Grade", col = "cyan")

boxplot(moody$PARTICIPATION ~ moody$GRADE, main="Participation vs Grades", 
        xlab="Grade", ylab="Participation", col=c("cyan", "red"))

boxplot(moody$SCORE ~ moody$DOZES_OFF, main="Scores vs Dozes Off Frequency", 
        xlab="Dozes Off", ylab="Score", col=c("cyan", "red"))

boxplot(moody$SCORE ~ moody$TEXTING_IN_CLASS, 
        main="Scores vs Texting In Class Frequency", xlab="Texting in Class", 
        ylab="Score", col=c("cyan", "red"))