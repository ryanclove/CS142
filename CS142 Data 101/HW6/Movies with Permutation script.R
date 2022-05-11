# Ryan Coslove rmc326
#HW6 - Permutation

#---------------------------------#
# Low Gross Comedy Movies have higher imdb scores than High Gross Action Movies

#data clean and subset, either
Com.Act.data <- subset(Movies, (Movies$genre == "Comedy" & Movies$Gross == "Low") 
                       | (Movies$genre == "Action" & Movies$Budget == "High"))
Com.data <- subset(Movies, Movies$genre == "Comedy" & Movies$Gross == "Low")
Act.data <- subset(Movies, Movies$genre == "Action" & Movies$Budget == "High")

#imdb 
Com.imdb <- Com.data$imdb_score
Act.imdb <- Act.data$imdb_score

#means of two samples
mean.Com <- mean(Com.imdb)
mean.Act <- mean(Act.imdb)

#get permutation
PermutationTestSecond::Permutation(Com.Act.data, "genre","imdb_score",10000,
                                   "Comedy", "Action")

#P-value
# 1st - p = 2e-04
# 2nd - p = 6e-04
# 3rd - p = 7e-04