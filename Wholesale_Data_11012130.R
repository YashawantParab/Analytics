#K-Mean of Wholesale customer data

#Loading required data
dataset <-  read.csv("Wholesale customers data.csv")

set.seed(20)
res1 <- kmeans(dataset[,3:6],1,nstart=20)
res2 <- kmeans(dataset[,3:6],2,nstart=20)
res3 <- kmeans(dataset[,3:6],3,nstart=20)
res4 <- kmeans(dataset[,3:6],4,nstart=20)
res5 <- kmeans(dataset[,3:6],5,nstart=20)
res6 <- kmeans(dataset[,3:6],6,nstart=20)

#WCSS is use to calculat within cluster 
wss <- c(sum(res1$withinss),sum(res2$withinss), sum(res3$withinss), sum(res4$withinss),
         +      sum(res5$withinss), sum(res6$withinss))
names(wss) <- 1:6
barplot(wss)

set.seed(20)
wccluster <- kmeans(dataset[,3:6],3,nstart=20)
wccluster
#Summary: Cluster vector defineswhich data points goes into which denderogram
#All 440 datapoints move to clustsers
table(wccluster$cluster, dataset$Fresh)
wccluster$cluster <- as.factor(wccluster$cluster)


wccluster <- kmeans(dataset[,3:6],3,nstart=20)
wccluster$betweenss
#with the use of formula of cluster betweeness 
wccluster$withinss
#with the use of formula of cluster betweeness 
wccluster$tot.withinss
#with the use of formula of cluster betweeness 
wccluster$totss

wss <- c(sum(res1$withinss),sum(res2$withinss), sum(res3$withinss), sum(res4$withinss),
         +      sum(res5$withinss), sum(res6$withinss))
names(wss) <- 1:6
barplot(wss)

#
## According to the barplot we need to choose at least 3 clusters


