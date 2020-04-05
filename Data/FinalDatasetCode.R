# K-Means Clustering on Cardatset
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


#######################################################################
#######################################################################

#Importing Diet Dataset for ANOVA
dataset <-  read.csv("DietDataset.csv",header=TRUE)
#Taking Required Data which i need to oredict the diet and the weight depend on age 
dataset <- dataset[3:7]
View(dataset)

#Fit the model in Lm function
#LM function represent the linear regression model 
fit <-  lm(weight6weeks ~ ., data=dataset)
#convertiing in to the data.frame i.r data is in data frame structure
nd<- data.frame(Age=c(23,55,39), 
                Height= c(170,185, 163), 
                pre.weight = c(60,78,59),
                Diet = c(2,1,3))
#Predict function is used to predict the fit model with the newdata nd i.e dataframe 
nd$Prediction <-  predict(fit,newdata=nd)
nd
#In this data set facing error regarding the non-factor data 
#so, Converted this data in factors
#converted data 1) pre.weight
#               2) Diet
dataset$pre.weight <-as.integer(dataset$pre.weight)
dataset$pre.weight <-as.factor(dataset$pre.weight)

dataset$Diet <-as.integer(dataset$Diet)
dataset$Diet <-as.factor(dataset$Diet)
#Fit an analysis of variance model to lm for each stratum
fit_anova <- aov(weight6weeks ~ pre.weight+Diet, data = dataset)
summary (fit_anova)

# Tukey compares the various means in parallel
TukeyHSD(fit_anova)

# #BoxPlot
# DietData$weightlost<-DietData$pre.weight-DietData$weight6weeks
# boxplot(DietData$weightlost~DietData$Diet,main='Weight Lost by Diet', xlab='Diet',
#         ylab='Weight Lost')
#Summary
#Summary: F(29,2) actual result F value is 36.055 an the p value is <2e-16 is ery small and
# small than 0.05 so the result is significant


#########################################################################
#########################################################################

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


###########################################################


