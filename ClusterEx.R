mydata1 <- read.csv("D:/Shilpa/Datascience/classroom/dataset/Universities.csv")
mydata <- scale(mydata1[,2:7])

d <-dist(mydata,method = "euclidean")#computing the distance matrix
fit <- hclust(d,method="centroid") #average Building the algorithm
plot(fit)#display dendogram
groups<- cutree(fit,k=4) #cut tree into 4 cluster

rect.hclust(fit,k=4,border="red")
clusters = data.frame('Uni'=mydata1[,1],'Cluster'=groups)
clusters
