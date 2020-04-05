# K-Means Clustering
# Importing the dataset
dataset = read.csv("cars.csv")
View(dataset)
X <- dataset[2]
View(X)
#using elbow method to find the optical number of cluster'
set.seed(123)
wcss <- vector()
#for (i in 1:50) wcss[i] <- sum(kmeans(X, i)$withinss)
#plot(1:50, wcss, type = "b", main = paste("Clusters of clients"), xlab = "Number of clustes", ylab = "WCSS")

#Applying Kmeans to mall Dataset
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)


#Visualising the cluster
install.packages("cluster")
library(cluster)
clusplot(X, 
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("CarData"),
         xlab = "Annual Income",
         ylab = "spending score")
