#Scatter Plot of Valuation of Company vs Company Asked For

shark <- read.csv("Shark_Tank_Companies.csv")

plot(shark$askedFor, shark$valuation, main = "Valuations of Comapnies vs What
       Was Asked FOr", xlab = "Amount Asked For", ylab = "Valuation", 
       xlim = c(1000, 5000000), ylim = c(40000, 30000000), col = 'purple')

##########

#Barplot of Category of Company vs Stake Exchanged
t <- table(shark$category)

barplot(t,xlab="Category of Company",ylab="Stake Exchanged",col= rainbow(7),
        main="Barplot for Category of Company vs Stake Exchanged",
        border="black")

##########
#Moasicplot of Mutliple Entrepreneurs vs Season Number
mosaicplot(shark$season~shark$Multiple.Entreprenuers, col = rainbow(7), 
           main = "Multiple Entreprenuers vs Season Number", 
           xlab = "Season Number", 
           ylab = "True or False of Multiple Entrepreneurs" )
