#importing dataset
dataset = read.csv("Mall_Customers.csv")
X = dataset[4:5]

#using the dendrogram to find the optimal number of clusters
dendrogram = hclust(dist(X, method = "euclidean"), method = "ward.D")
plot(dendrogram,
     main = paste("Dendrogram"),
     xlab = "Customers",
     ylab = "Euclidean Distannce")

#Fitting Hierarchical clustering algo
HC = hclust(dist(X, method = "euclidean"), method = "ward.D")
#use cut tree method
y_hc = cutree(HC, 5, )

#Visualising the cluster
clusplot(X, 
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of client"),
         xlab = "Annual Income",
         ylab = "spending score")