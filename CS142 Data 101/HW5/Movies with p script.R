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

# standard deviation of two samples.
sd.R <- sd(R.imdb)
sd.PG <- sd(PG.imdb)

#length of R and PG
len_R <- length(R.imdb)
len_PG <- length(PG.imdb)

#standard deviation of difference traffic
sd.R.PG <- sqrt(sd.R^2/len_R + sd.PG^2/len_PG)

#means of two samples
mean.R <- mean(R.imdb)
mean.PG <- mean(PG.imdb)

#z score
zeta <- (mean.R - mean.PG)/sd.R.PG

#plot red line
plot(x=seq(from = -12, to= 12, by=0.1),y=dnorm(seq(from = -12, to= 12, by=0.1),
                                               mean=0), type='l', 
                                  xlab = 'mean difference', ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 1-pnorm(zeta)
#p = 0


#---------------------------------#
# High Budget Sci-Fi movies have lower imdb scores than High Budget Family

#data clean and subset, either
Sci.data <- subset(Movies, Movies$genre == "Sci-Fi" & Movies$Budget == "High")
Fam.data <- subset(Movies, Movies$genre == "Family" & Movies$Budget == "High")

#imdb 
Sci.imdb <- Sci.data$imdb_score
Fam.imdb <- Fam.data$imdb_score

# standard deviation of two samples.
sd.Sci <- sd(Sci.imdb)
sd.Fam <- sd(Fam.imdb)

#length of R and PG
len_Sci <- length(Sci.imdb)
len_Fam <- length(Fam.imdb)

#standard deviation of difference traffic
sd.Sci.Fam <- sqrt(sd.Sci^2/len_Sci + sd.Fam^2/len_Fam)

#means of two samples
mean.Sci <- mean(Sci.imdb)
mean.Fam <- mean(Fam.imdb)

#z score
zeta <- (mean.Sci - mean.Fam)/sd.Sci.Fam

#plot red line
plot(x=seq(from = -10, to= 10, by=0.1),y=dnorm(seq(from = -10, to= 10,  
                                                   by=0.1), mean=0),
     type='l', xlab = 'mean difference', ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 1-pnorm(zeta)
#p = 2.22044e-16

#---------------------------------#
# Low Gross Comedy Movies have higher imdb scores than High Gross Action Movies

#data clean and subset, either
Com.data <- subset(Movies, Movies$genre == "Comedy" & Movies$Gross == "Low")
Act.data <- subset(Movies, Movies$genre == "Action" & Movies$Budget == "High")

#imdb 
Com.imdb <- Com.data$imdb_score
Act.imdb <- Act.data$imdb_score

# standard deviation of two samples.
sd.Com <- sd(Com.imdb)
sd.Act <- sd(Act.imdb)

#length of R and PG
len_Com <- length(Com.imdb)
len_Act <- length(Act.imdb)

#standard deviation of difference traffic
sd.Com.Act <- sqrt(sd.Com^2/len_Com + sd.Act^2/len_Act)

#means of two samples
mean.Com <- mean(Com.imdb)
mean.Act <- mean(Act.imdb)

#z score
zeta <- (mean.Act - mean.Com)/sd.Com.Act
#plot red line
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  
                                                   by=0.1), mean=0),
     type='l', xlab = 'mean difference', ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 1-pnorm(zeta)
#p = 0.00014989
