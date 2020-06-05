#################################Hypothesis Testing#####################################

#Problem Statement:Fantaloons Sales managers commented that % of males versus females 
#walking in to the store differ based on day of the week. Analyze the data and determine 
#whether there is evidence at 5 % significance level to support this hypothesis.

# Data : Faltoons.csv
#####################################################################################

faltoonData <- read.csv(file.choose()) #Faltoons.csv
attach(faltoonData)
View(faltoonData)

table1 <- table(Weekdays,Weekend)
View(table1)
table1

weekdays <- (table(Weekdays))
weekdays

#Female   Male 
#287    113 

weekend <- table(Weekend)
weekend

#Female   Male 
#233    167

# Both input and output variables are discrete.We have 2 population to compare so we
#procced with 2- proportion test.

#Ho : Proportions of male and female in weekdays are same.
#Ha : Proportions of male and female in weekdays are different.

prop.test(x=c(287,167),n=c(520,270),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions and unequal proportions of Male and Femal for weekdays
# p-value = 0.07254 > 0.05 so p high null fly => accept null hypothesis.

#Hence proportions of males and female on weekdays are same.

#Ho : Proportions of male and female in weekend are same.
#Ha : Proportions of male and female in weekend are different.

prop.test(x=c(233,113),n=c(520,270),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions and unequal proportions of Male and Femal for weekdays
# p-value = 0.4271 > 0.05 so p high null fly => accept null hypothesis.

#Hence proportions of males and female on weekend are same.

#Hence we conclude that the proportion of males and females are same on weekdays and weekend respectively.















