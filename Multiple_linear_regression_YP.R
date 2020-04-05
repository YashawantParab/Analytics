dataset = read.csv("Mall_Customers.csv")
str(dataset)
head(dataset)
#scatterplot using pairs
pairs(dataset)
data1 <- as.data.frame(cbind(as.numeric(dataset$CustomerID),as.numeric(dataset$Genre),as.numeric(dataset$Age),
                             
                             as.numeric(dataset$Annual.Income..k..),as.numeric(dataset$Spending.Score..1.100.))) #,dis_data$V5))
head(data1)
#Correlation matrix
cor(data1)
pairs(data1)
str(data1)
#how to generate estimate coefficients with multiple variable
#define model with lm
m1 <- lm(V5 ~ V4+ V3+ V2+ V1, data = data1)
summary(m1)
#prediction
predict(m1, data.frame("V4"= 15, "V3"=18, "V2"=2, "V1"=2))
 