# PCA Example


library(gdata)
PCA <- read.csv("/Users/Dell/Desktop/Universities.csv")
pca <- princomp(PCA[,2:7],cor = TRUE, scores = TRUE,covmat = NULL)
pca <- prin
summary(pca)

pca$scores

plot(pca$scores[,1:2],col="Blue",pch=18,cex=0.5,lwd=3)
text(pca$scores[,1:2],labels=c(1:25),cex=1)
