############################################################################
######################## EastWestAirlines ##################################
############################################################################
library(readr)
library(readxl)

eastWestAirlines <- read_excel("D:/Shilpa/Datascience/Assignments/ClusteringAssignment/EastWestAirlines.xlsx",1)
##EastWestAirlines.xlsx

eastWestAirlines<-read_excel(file.choose(),1)
View(eastWestAirlines)
  
normEastWestAirlines <- scale(eastWestAirlines[,2:12])

distEastWestAirlines <- dist(normEastWestAirlines,method = "euclidean")

#Calculate distance between clusters using complete linkage
airlineClust <- hclust(distEastWestAirlines,method = "complete")
plot(airlineClust)
groups <- cutree(airlineClust,k=3)
rect.hclust(airlineClust,k=3,border="red")
airlinesFinal <- cbind(eastWestAirlines,groups)
colnames(airlinesFinal)

aggregate(eastWestAirlines[,2:12],FUN = mean,by=list(airlinesFinal$groups))


######################################################################################
# 2nd approach reading using read_xlsx 
library(data.table)
library(readxl)

eastWestAirlines <- read_xlsx("D:/Shilpa/Datascience/Assignments/ClusteringAssignment/EastWestAirlines.xlsx",sheet="data")
ncol(eastWestAirlines)
head(eastWestAirlines)
# A tibble: 6 x 12
#`ID#` Balance Qual_miles cc1_miles cc2_miles cc3_miles Bonus_miles Bonus_trans
#<dbl>   <dbl>      <dbl>     <dbl>     <dbl>     <dbl>       <dbl>       <dbl>
# 1     1   28143          0         1         1         1         174           1
#2     2   19244          0         1         1         1         215           2
#3     3   41354          0         1         1         1        4123           4
#4     4   14776          0         1         1         1         500           1
#5     5   97752          0         4         1         1       43300          26
#6     6   16420          0         1         1         1           0           0
# ... with 4 more variables: Flight_miles_12mo <dbl>, Flight_trans_12 <dbl>,
#   Days_since_enroll <dbl>, `Award?` <dbl>
eastWestAirlines <- na.omit(eastWestAirlines)
head(eastWestAirlines)
normEastWestAirlines <- scale(eastWestAirlines[,2:12])
#distEastWestAirlines <- dist(normEastWestAirlines,method = "euclidean")

#Calculate distance between clusters using Average linkage
#airlineClust <- hclust(distEastWestAirlines,method = "complete")
#plot(airlineClust,cex=0.6,hang=-1)


#################### Using K-Means#########################################
airlineKmeans <- kmeans(normEastWestAirlines,5)
str(airlineKmeans)
summary(airlineKmeans)
airlineKmeans$centers
eastWestAirlines <- cbind(eastWestAirlines,airlineKmeans$cluster)
colnames(eastWestAirlines)

install.packages("cluster")
library(cluster)

xcl <- clara(normEastWestAirlines,5)
clusplot(xcl)

