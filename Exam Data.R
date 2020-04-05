#Importing Diet Dataset
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

#Summary
#Summary: F(29,2) actual result F value is 36.055 an the p value is <2e-16 is ery small and
# small than 0.05 so the result is significant


