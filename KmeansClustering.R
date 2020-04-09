install.packages("plyr")
library(plyr)

x <- runif(50) #generating random variable from uniform distribution
y <- runif(50)

data <- cbind(x,y)
plot(data)

#Elbow chart
wss <-c()

for(i in 2:15) wss[i] <- sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type="b",xlab="No of cluster",ylab="Avg distance")

km <- kmeans(data,10)
km$centers
km$cluster

install.packages("animation")
library(animation)
windows()

km <- kmeans.ani(data,5)

