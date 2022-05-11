# Ryan Coslove rmc326
# HW7 - Data Driven Blog

shark <- read.csv("Shark_Tank_Companies.csv")

# Successful deals have a higher mean exchange for stake than unsuccessful deals

#data clean and subset, either
Deal.data <- subset(shark, shark$deal == "True")
NoDeal.data <- subset(shark, shark$deal == "False")

#Stake
Deal.Stake <- Deal.data$exchangeForStake
NoDeal.Stake <- NoDeal.data$exchangeForStake

# standard deviation of two samples.
sd.Deal <- sd(Deal.Stake)
sd.NoDeal <- sd(NoDeal.Stake)

#length of Deal and NoDeal
len_Deal <- length(Deal.Stake)
len_NoDeal <- length(NoDeal.Stake)

#standard deviation of difference
sd.Deal.NoDeal <- sqrt(sd.Deal^2/len_Deal + sd.NoDeal^2/len_NoDeal)

#means of two samples
mean.Deal <- mean(Deal.Stake)
mean.NoDeal <- mean(NoDeal.Stake)

#z score
zeta <- (mean.NoDeal - mean.Deal)/sd.Deal.NoDeal

#plot red line
plot(x=seq(from = -3, to= 3, by=0.1),y=dnorm(seq(from = -3, to= 3, by=0.1),
          mean=0), type='l', xlab = 'mean difference', ylab='possibility')

abline(v=zeta, col='red')

#get p
p = 1-pnorm(zeta)
# p = 0.0294

t <- table(shark$deal)

boxplot(shark$exchangeForStake ~ shark$deal, xlab = "Deal Was Made", ylab = 
          "Exchanged Stake", main = "Exchanged Stake vs Successful Deal",
        col = "cyan")

#----------------------------------------#

# Valuations are higher for companies that asked for less than or equal to 
#$150,000 than valuations for companies that asked for more than $150,0000

#data clean and subset, either
Less.data <- subset(shark, shark$askedFor <= 150000)
More.data <- subset(shark, shark$askedFor > 150000)

#Valuation
Less.valuation <- Less.data$valuation
More.valuation <- More.data$valuation

# standard deviation of two samples.
sd.Less <- sd(Less.valuation)
sd.More <- sd(More.valuation)

#length of Deal and NoDeal
len_Less <- length(Less.valuation)
len_More <- length(More.valuation)

#standard deviation of difference
sd.Less.More <- sqrt(sd.Less^2/len_Less + sd.More^2/len_More)

#means of two samples
mean.Less <- mean(Less.valuation)
mean.More <- mean(More.valuation)

#z score
zeta <- (mean.More - mean.Less)/sd.Less.More

#plot red line
plot(x=seq(from = -3, to= 3, by=0.1),y=dnorm(seq(from = -3, to= 3, by=0.1),
              mean=0), type='l', xlab = 'mean difference', ylab='possibility')

abline(v=zeta, col='red')

#get p
p = 1-pnorm(zeta)
# p = 0.0000

boxplot(shark$valuation ~ (shark$askedFor <= 150000), 
        xlab = "Asked for <= $150,000", ylab = 
          "Valuation", main = "Asked For vs Valuation", col = "cyan")
