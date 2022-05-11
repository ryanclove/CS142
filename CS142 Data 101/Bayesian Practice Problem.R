# Ryan Coslove rmc326

# 1. What are the odds that a movie with imdb rating over 7  belongs to  History genre?
#Belief - Movie belongs to history genre
#Observation - IMDB score is over 7
Movies
Prior<-nrow(Movies[Movies$genre =='History',])/nrow(Movies)
Prior # 0.1911513
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds # 0.24
TruePositive<-round(nrow(Movies[Movies$imdb_score > 7 & Movies$genre=='History',])/nrow(
  Movies[Movies$genre =='History',]),2)
TruePositive # 0.71
FalsePositive<-round(nrow(Movies[Movies$imdb_score > 7 & Movies$genre !='History',])/nrow(Movies[Movies$genre !='History',]),2)
FalsePositive # 0.27
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio # 2.63
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds # 0.6312
Posterior <-PosteriorOdds/(1+PosteriorOdds)
round(Posterior,2) # 0.39


moody
# 2. What are the odds that a student who  texts "all the time"  scores above 80 in professor Moody's class
#Belief - Student texts all the time
#Observation - Scores above an 80
Prior<-nrow(moody[moody$TEXTING_IN_CLASS =='always',])/nrow(moody)
Prior # 0.2149758
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds # 0.27
TruePositive<-round(nrow(moody[moody$SCORE > 80 & moody$TEXTING_IN_CLASS =='always',])/nrow(
  moody[moody$TEXTING_IN_CLASS =='always',]),2)
TruePositive # 0.11
FalsePositive<-round(nrow(moody[moody$SCORE > 80 & moody$TEXTING_IN_CLASS !='always',])/nrow(moody[moody$TEXTING_IN_CLASS !='always',]),2)
FalsePositive # 0.24
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio # 0.46
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds # 0.1242
Posterior <-PosteriorOdds/(1+PosteriorOdds)
round(Posterior,2) # 0.11

moody
# 3. What are the odds of getting an A for students who doze "all the time"?
#Belief - Student doze all the time
#Observation - Students get an A
Prior<-nrow(moody[moody$DOZES_OFF =='always',])/nrow(moody)
Prior # 0.2077295
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds # 0.26
TruePositive<-round(nrow(moody[moody$GRADE == "A" & moody$DOZES_OFF =='always',])/nrow(
  moody[moody$TEXTING_IN_CLASS =='always',]),2)
TruePositive # 0.19
FalsePositive<-round(nrow(moody[moody$GRADE == "A" & moody$DOZES_OFF!='always',])/nrow(moody[moody$DOZES_OFF !='always',]),2)
FalsePositive # 0.19
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio # 1
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds # 0.26
Posterior <-PosteriorOdds/(1+PosteriorOdds)
round(Posterior,2) # 0.21