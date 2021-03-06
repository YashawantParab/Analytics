dataset <- read.csv("Mall_Customers.csv")
X <- dataset[4:5]

#using elbow method to find the optical number of cluster'
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X, i)$withinss)
plot(1:10, wcss, type = "b", main = paste("Clusters of clients"), xlab = "Number of clustes", ylab = "WCSS")

#Applying Kmeans to mall Dataset
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

#Visualising the cluster
#library(cluster)
clusplot(X, 
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of client"),
         xlab = "Annual Income",
         ylab = "spending score")