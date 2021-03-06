#Association Rules
library(arules)

Titanic <- read.csv("/Users/Dell/Desktop/Titanic.csv")

Titanic <- Titanic[,-c(1)]
rules <- apriori(Titanic)
arules::inspect(rules)

#By lift measure
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)


rules <- apriori(Titanic,parameter=list(minlen=1,supp=0.1,conf=0.5),
                 appearance=list(rhs=c("Survived=No","Survived=Yes")),
                                 control=list(verbose=F))

arules::inspect(rules)


