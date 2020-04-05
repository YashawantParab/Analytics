## Start with cluster algos with iris dataset
## load dataset and rename columns
require(graphics); require(utils)
library(ggplot2)

dataset=(iris)

summary(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Name)) + geom_point()

pairs(iris,col=iris$X )

#############################################################

## code for k-means

set.seed(20)
iriscluster <- kmeans(iris[,3:43,nstart=20)
iriscluster
table(iriscluster$cluster, iris$X)
iriscluster$cluster <- as.factor(iriscluster$cluster)


iriscluster <- kmeans(iris[,3:43,nstart=20)
iriscluster$betweenss
iriscluster$withinss
iriscluster$tot.withinss
iriscluster$totss

## farbliche Darstellung der Clusterzugehörigkeit von jedem Datapoint
plot(iris[c("Petal.Length","Petal.Width")col=iriscluster$cluster)



## Auswertung von kmeans, wie viele Cluster sollten wir bilden?
res1 <- kmeans(iris[,3:41,nstart=20)
res2 <- kmeans(iris[,3:42,nstart=20)
res3 <- kmeans(iris[,3:43,nstart=20)
res4 <- kmeans(iris[,3:44,nstart=20)
res5 <- kmeans(iris[,3:45,nstart=20)
res6 <- kmeans(iris[,3:46,nstart=20)

wss <- c(sum(res1$withinss),sum(res2$withinss), sum(res3$withinss), sum(res4$withinss),
         +      sum(res5$withinss), sum(res6$withinss))
names(wss) <- 1:6
barplot(wss)

## According to the barplot we need to choose at least 3 clusters


#############################################################
## code for dbscan

# install.packages("dbscan")
library(dbscan)


## first we need to find suitable epsilon parameter. We do this using a k-NN plot for k = dim +1
data(iris)
iris_matrix <-  as.matrix(iris[,1:4])
kNNdistplot(iris_matrix, k=4)
abline(h=0.4, col="red", lty=2)


# now run dbscan 
set.seed(1234)

res <-  dbscan(iris_matrix,eps=.4,minPts = 4)
res

pairs(iris_matrix,col=res$cluster +1L)

points(iris_matrix[res$cluster==0, pch = 3, col = "grey")

# Display the hull plot
hullplot(iris_matrix, res$cluster)

##########################################################
## hierarchical clustering


iris_dat <- iris[, -5]

# perform hierarchical clustering
hc <- hclust(dist(iris_dat),"ave")
# show the dendogram as string output
(dend1 <- as.dendrogram(hc))
str(dend1)          # "str()" method
str(dend1, max = 2, last.str =  "'") # only the first two sub-levels
# plot the dendrogram

plot(hc,hang=-2)

# install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
hclust(dist(iris_dat),"ave")


# different types of plots
# fan
plot(as.phylo(hc), type = "fan")

# Unrooted
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

# Cut the dendrogram into 3 clusters
colors = c("red", "blue", "green", "black")
clus3 = cutree(hc, 3)
plot(as.phylo(hc), type = "unrooted", tip.color = colors[clus3
     label.offset = .5, cex = 0.7)
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5, tip.color = colors[clus3])
hc

# how many does it get wrong
clusGroup <- cutree(hc, k=3)
sum(clusGroup != as.numeric(iris$Species))

#########################################################################
\
