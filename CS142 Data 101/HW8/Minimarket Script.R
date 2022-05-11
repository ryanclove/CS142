# Ryan Coslove rmc326
#HW8 - Minimarket

#---------------------------------#

Minimarket <- read.csv("~/Senior Year/Spring 2022/CS142 Minimarket 101/Minimarket.csv")

table(Minimarket$BREAD,Minimarket$BUTTER)
table(Minimarket$BREAD,Minimarket$COOKIES)
table(Minimarket$BREAD,Minimarket$COFFEE)
table(Minimarket$BREAD,Minimarket$TEA)
# All pretty closely bought together

table(Minimarket$BUTTER,Minimarket$COOKIES)
table(Minimarket$BUTTER,Minimarket$COFFEE)
table(Minimarket$BUTTER,Minimarket$TEA)
# Also pretty closely bought together

table(Minimarket$COOKIES,Minimarket$COFFEE)
table(Minimarket$COOKIES,Minimarket$TEA)
# Same as before

table(Minimarket$COFFEE,Minimarket$TEA)
# Same

PermutationTestSecond::Permutation(Minimarket, "BUTTER", "BREAD",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "COOKIES", "BREAD",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "COFFEE", "BREAD",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "TEA", "BREAD",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "BUTTER", "COOKIES",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "BUTTER", "TEA",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "BUTTER", "COFFEE",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "COOKIES", "TEA",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "COOKIES","COFFEE",1000, "0", "1")
PermutationTestSecond::Permutation(Minimarket, "TEA", "COFFEE",1000, "0", "1")
