#Ryan Coslove rmc326
#HW 12 Homework Market - Boundless testing

minimarket <- read.csv("HomeworkMarket2022.csv", sep = ",")
View(minimarket)

minimarket$IN<-'Out_Slice'
minimarket[minimarket$Beer=='Lager' & minimarket$Day=='Weekend' &  minimarket$Snacks =='Crackers', ]$IN<-'In_Slice'
d<-table(minimarket$Location, minimarket$IN)
chisq.test(d)
# X-squared = 157.51, df = 3, p-value < 2.2e-16

minimarket$IN<-'Out_Slice'
minimarket[minimarket$Beer=='Lager' & minimarket$Location =='Princeton' &  minimarket$Snacks =='Popcorn', ]$IN<-'In_Slice'
d<-table(minimarket$SoftDrinks, minimarket$IN)
chisq.test(d)
# X-squared = 79.81, df = 3, p-value < 2.2e-16

minimarket$IN<-'Out_Slice'
minimarket[minimarket$Day =='Weekday' & minimarket$Location =='Princeton' &  minimarket$Snacks =='Popcorn', ]$IN<-'In_Slice'
d<-table(minimarket$SoftDrinks, minimarket$IN)
chisq.test(d)
# X-squared = 205.31, df = 3, p-value < 2.2e-16

minimarket$IN<-'Out_Slice'
minimarket[minimarket$SoftDrinks =='Cola' & minimarket$Sweets =='Twix' &  minimarket$Snacks =='Popcorn', ]$IN<-'In_Slice'
d<-table(minimarket$Location, minimarket$IN)
chisq.test(d)
# X-squared = 43.048, df = 3, p-value = 2.404e-09

minimarket$IN<-'Out_Slice'
minimarket[minimarket$SoftDrinks =='Cola' & minimarket$Location =='Princeton' &  minimarket$Day =='Weekday', ]$IN<-'In_Slice'
d<-table(minimarket$Snacks, minimarket$IN)
chisq.test(d)
# X-squared = 400.8, df = 4, p-value < 2.2e-16