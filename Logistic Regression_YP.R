url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
dataURL<- read.csv(url, header = FALSE)
View(dataURL)
head(dataURL)
colnames(dataURL) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chor",
  "fbs",
  "restecg",
  "thalach",
   "exang",
   "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd")
str(dataURL)
data[dataURL == "?"] <-NA
data[dataURL$sex ==0,]$sex <- "F"
data[dataURL$sex ==1,]$sex <- "M"
#by if condition
#dataURL$sex <- ifelse(test = dataURL$sex == 0, yes = "F", no = "M")
dataURL$sex <-as.factor(dataURL$sex)
dataURL$cp <-as.factor(dataURL$cp)
dataURL$fbs <-as.factor(dataURL$fbs)
dataURL$restecg <-as.factor(dataURL$restecg)
dataURL$exang <-as.factor(dataURL$exang)
dataURL$slope <-as.factor(dataURL$slope)
dataURL$hd <- ifelse(test = dataURL$hd == 1, yes = "Healthy", no = "Diseased")
#View(dataURL)

dataURL$ca <-as.integer(dataURL$ca)
dataURL$ca <-as.factor(dataURL$ca)

dataURL$thal <-as.integer(dataURL$thal)
dataURL$thal <-as.factor(dataURL$thal)

dataURL$hd = factor(dataURL$hd, levels = c("Diseased","Healthy"))

str(dataURL)

#how many samples has NA value
nrow(dataURL[is.na(dataURL$ca) | is.na(dataURL$thal),])
dataURL[is.na(dataURL$ca) | is.na(dataURL$thal),]
 nrow(dataURL)

 #Remove NA values from the data
 dataFinal <- dataURL[!(is.na(data$ca) | is.na(dataURL$thal)),]
 nrow(dataURL) 
View(dataURL$hd)
 #now we make sure that health and diseased samples come frm each gender(female and male)
 xtabs(~hd + sex, data=dataFinal)
 xtabs(~hd + cp, data=dataFinal)
 xtabs(~hd + fbs, data=dataFinal)
 xtabs(~hd + restecg, data=dataFinal)
 xtabs(~hd + sex, data=dataFinal)
 xtabs(~hd + slope, data=dataFinal)
 xtabs(~hd + ca, data=dataFinal)
 xtabs(~hd + thal, data=dataFinal)
 
 #Generilized linear model by usiing glm
 #we want to use sex to predict heart disease (hd)
logistic <- glm(hd ~ sex, data=dataFinal, family = "binomial")
summary(logistic)
 
 