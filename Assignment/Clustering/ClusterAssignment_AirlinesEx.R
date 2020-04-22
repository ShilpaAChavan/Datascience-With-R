############################################################################
######################## EastWestAirlines ##################################
############################################################################
#Problem Statement:
#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.
############################################################################

library(readr)
library(readxl)

#eastWestAirlines <- read_excel("D:/Shilpa/Datascience/Assignments/ClusteringAssignment/EastWestAirlines.xlsx",sheet = "data")

eastWestAirlines<-read_excel(file.choose(),2)
View(eastWestAirlines)
head(eastWestAirlines)
normEastWestAirlines <- scale(eastWestAirlines[,2:12])

distEastWestAirlines <- dist(normEastWestAirlines,method = "euclidean")

#Calculate distance between clusters using complete linkage
airlineClust <- hclust(distEastWestAirlines,method = "ward.D2")
plot(airlineClust)
# 3 clusters are formed at height 85.

groups <- cutree(airlineClust,k=3)
table(groups)
rect.hclust(airlineClust,k=3,border="red")
airlinesFinal <- cbind(eastWestAirlines,groups)
colnames(airlinesFinal)

aggregate(eastWestAirlines[,2:12],FUN = mean,by=list(airlinesFinal$groups))
#Group.1   Balance Qual_miles cc1_miles cc2_miles cc3_miles Bonus_miles Bonus_trans
#      1  46718.86   9.274407  1.242266  1.023303  1.000000    5037.793    7.091201
#      2 116314.45 363.839130  3.498551  1.000000  1.035507   37150.357   18.066667
#      3 134880.89 393.323077  2.430769  1.000000  1.000000   36582.169   29.338462

#Flight_miles_12mo Flight_trans_12 Days_since_enroll    Award?
#          221.1671       0.7002812          3772.786 0.1880273
#          377.0000       1.1500000          4696.888 0.6630435
#         5915.5231      16.6384615          4599.608 0.7538462

#Interpretation: It seems the clusters are classified using balance,bonus miles and 
#days since enrolled.
# Cluster 1(Non frequent flyer : relatively recent passengers) :2489 total in the group
#the passengers are relatively new with low balance and  bonus miles.
# Cluster 3(Frequent flyer : old passengers) : 130 total in this group.These are old
#passengers with high balance and bonus miles, also with more Number of flight miles
#in the past 12 months
# Cluster 2 : passengers can be said as intermediate flyers.

#################### Using K-Means#########################################
airlineKmeans <- kmeans(normEastWestAirlines,5)
str(airlineKmeans)
summary(airlineKmeans)
airlineKmeans$centers


#Elbow chart
wss <-c()
for(i in 2:12) wss[i] <- sum(kmeans(normEastWestAirlines,centers = i)$withinss)
plot(1:12,wss,type="b",xlab="No of cluster",ylab="Avg distance")
#the distortion rate becomes constant is the optimal value.Here  k=3.

airlineKmeans <- kmeans(normEastWestAirlines,3)
summary(airlineKmeans)
airlineKmeans$centers
eastWestAirlines <- cbind(eastWestAirlines,airlineKmeans$cluster)
colnames(eastWestAirlines)
aggregate(eastWestAirlines[,2:12],FUN = mean,by=list(airlineKmeans$cluster))

#Group.1   Balance Qual_miles cc1_miles cc2_miles cc3_miles Bonus_miles Bonus_trans
#       1 116978.71  156.82049  3.731533  1.002383  1.034154   40417.577   18.960286
#       2  44370.06   96.83333  1.235820  1.019037  1.000389    4831.835    7.005439
#       3 197873.37  780.89157  2.150602  1.036145  1.030120   31562.446   27.066265
#Flight_miles_12mo Flight_trans_12 Days_since_enroll    Award?
#  342.4543       1.0119142          4884.592         0.6528991
#  200.6974       0.6262626          3704.443         0.2039627
#  5373.6024      15.7048193         4730.018         0.8072289

install.packages("animation")
library(animation)
windows()

km <- kmeans.ani(eastWestAirlines[2:12],3)


#Interpretation:
# Cluster 2(Non frequent flyer : relatively recent passengers) :2574 total in the group
#the passengers are relatively new with low balance and  bonus miles.
# Cluster 3(Frequent flyer : old passengers) : 166 total in this group.These are old
#passengers with high balance and bonus miles, also with more Number of flight miles
#in the past 12 months
# Cluster 1 (1259 passengers) : passengers can be said as intermediate flyers.

# Conclusion : Recently added passengers are more in number which are non frequent 
#flyer, discounts on fare other offerings should be given to improve the number fly. 
