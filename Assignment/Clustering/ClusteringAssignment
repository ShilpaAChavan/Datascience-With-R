  
######################################Crime Analysis####################################################
#Problem Statement:
#Perform Clustering for the crime data and identify the number of clusters 
#formed and draw inferences.
########################################################################################################

#Using Hierarchial clustering

crime_data <- read.csv(file.choose())
#crime_data <-read.csv("D:/Shilpa/Datascience/Assignments/ClusteringAssignment/crime_data.csv")
head(crime_data)
crimedata <- crime_data[2:5]
head(crimedata)

#Normalize the data
norm_crimedata <- scale(crimedata)
head(norm_crimedata)
d <- dist(norm_crimedata,method="euclidean")

#calculating distance between clusters using complete linkage.
fit <- hclust(d,method="complete")

#Plot dendogram
plot(fit)
groups <- cutree(fit,k=3)
rect.hclust(fit,k=3,border="blue")
crimedatafinal <- cbind(crime_data,groups)
crimedatafinal

aggregate(crimedatafinal[,2:6],by=list(crimedatafinal$groups),FUN = mean)

#Group.1    Murder  Assault UrbanPop     Rape groups
#1       1 14.087500 252.7500 53.50000 24.53750      1
#2       2 11.054545 264.0909 79.09091 32.61818      2
#3       3  5.003226 116.4839 63.83871 16.33871      3
  
# Summary : Group 2 Countries have higher rate of crime and Group 3 have 
# comparatively lower crime rate.

########################## With Kmeans method ##########################

install.packages("plyr")
library(plyr)

#Elbow chart
wss <-c()
for(i in 2:12) wss[i] <- sum(kmeans(norm_crimedata,centers = i)$withinss)
plot(1:12,wss,type="b",xlab="No of cluster",ylab="Avg distance")
#the distortion rate becomes constant is the optimal value.Here  k=4.

# k=4 optimal kvalue
crimeDataKmeans <- kmeans(norm_crimedata,4)
str(crimeDataKmeans)
summary(crimeDataKmeans)
crimeDataKmeans$centers
crimeData <- cbind(crime_data,crimeDataKmeans$cluster)
crimeData
colnames(crimeData)
crimeDataKmeans$centers
aggregate(crimeData[,2:6],by=list(crimeDataKmeans$cluster),FUN = mean)

#Group.1   Murder   Assault    UrbanPop Rape crimeDataKmeans$cluster
#1         13.93750 243.62500  53.75000 21.41250                       1
#2         5.65625  138.87500  73.87500 18.78125                       2
#3         3.60000   78.53846  52.07692 12.17692                       3
#4         10.81538  257.38462 76.00000 33.19231                       4

install.packages("animation")
library(animation)
windows()

km <- kmeans.ani(crimedata,4)

#Interpretation : Group 4 countries have higher crime rate with higher assaults as well
#there urban population is dense followed by Group 1 countries, then group 2 and
#group3.

