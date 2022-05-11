# Ryan Coslove rmc326

Movies2022F.3 <- 
  read.csv("~/Senior Year/Spring 2022/CS142 Data 101/Movies2022F-3.csv")
View(Movies2022F.3)
Movies <- Movies2022F.3
View(Movies)
summary(Movies)

# High Budget R movies have lower imdb scores than High Budget PG movies
#data clean and subset, either
R.data <- subset(Movies, Movies$content == "R" & Movies$Budget == "High")
PG.data <- subset(Movies, Movies$content == "PG" & Movies$Budget == "High")

#imdb 
R.imdb <- R.data$imdb_score
PG.imdb <- PG.data$imdb_score

#Mean
mean.R <- mean(R.imdb)
mean.PG <- mean(PG.imdb)
means <- c(mean.R, mean.PG)

barplot(means,xlab="Movie Rating",ylab="IMDB Mean",col= rainbow(7),
        main="Barplot for Rating vs IMDB Mean", names.arg = c("R", "PG"),
        border="black")

#Movies in USA have lower IMDB scores than Movies in the UK
usauk.data <- subset(Movies, Movies$country == "USA" | Movies$country == "UK")
usa.data <- subset(usauk.data, usauk.data$country == "USA")
uk.data <- subset(usauk.data, usauk.data$country == "UK")
table(usauk.data$country)

#imdb 
usa.imdb <- usa.data$imdb_score
uk.imdb <- uk.data$imdb_score
imdbs <- c(usa.imdb, uk.imdb)

boxplot(imdbs~country,data=usauk.data,xlab="Country",ylab="IMDB Score", 
        main="Boxplot of Country vs IMDB",col=rainbow(7),border="black")

#Movies that have a high gross have higher imdb scores than movies with a low 
#gross
gross.data <- subset(Movies, Movies$Gross == "High" | Movies$Gross == "Low")
highG.data <- subset(Movies, Movies$Gross == "High")
lowG.data <- subset(Movies, Movies$Gross == "Low")
table(gross.data$Gross)

#imdb 
highG.imdb <- highG.data$imdb_score
lowG.imdb <- lowG.data$imdb_score
imdbsgross <- c(highG.imdb, lowG.imdb)

boxplot(imdbsgross~Gross,data=gross.data,xlab="Gross Type",ylab="IMDB Score", 
        main="Boxplot of Gross Type vs IMDB",col=rainbow(7),border="black")