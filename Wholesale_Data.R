Dataset <-  read.csv("Wholesale customers data.csv", header=TRUE)
View(Dataset)
summary(Dataset)
#seting a query on graphical parameters
par(mfrow=c(2,3))
#for loop on the dataset
for (i in c(3:8))
  hist(Dataset[,c(i)], breaks = 200, main = colnames(Dataset)[i], 
       xlab="Annual Spending (m.u.)", ylab = "No. of Customers")
#Normalng the data and taking the sequence of vectors
NormalizedData <- cbind(Dataset[,c(1,2)], scale(Dataset[,c(3:8)]))
summary(NormalizedData)
d_raw <- dist(Dataset[,c(1:8)])
d_nom <- dist(NormalizedData)
#d_dis <- dist(RawData[,c(1,2,9:14)])
hc_raw <- hclust(d_raw, method = "complete")
hc_nom <- hclust(d_nom, method = "complete")
#hc_dis_complete <- hclust(d_dis, method = "complete")
par(mfrow=c(1,3))
plot(hc_raw)
plot(hc_nom)
plot(hc_dis_complete)