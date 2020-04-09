
##################### Hierarchial Clustering #################################
mydata1 <- read.csv("D:/Shilpa/Datascience/classroom/dataset/Universities.csv")
mydata <- scale(mydata1[,2:7])

#computing the distance matrix
d <-dist(mydata,method = "euclidean")

#Building the algorithm.
#distance between clusters calculated using Centroid Method(Average method)
fit <- hclust(d,method="centroid") 

#display dendogram
plot(fit)

#cut tree into 4 cluster
groups<- cutree(fit,k=4) 

rect.hclust(fit,k=4,border="red")
clusters = data.frame('Uni'=mydata1[,1],'Cluster'=groups)
clusters
