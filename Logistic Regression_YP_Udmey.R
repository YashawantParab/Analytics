#logistic regresssion

dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[, 3:5]

library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set =  subset(dataset, split == FALSE)

#Feature Scaling
training_set[,1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[,1:2])

#fitting logistic regression on Training set using GLm (Generalized Linear Models)

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data =  training_set)
#predicting the test set results
#Predicting probability on based on glm classsifier
#response : gives probabilty listed in a single vector

prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
#vector of prected result
#ifelse to transform probabilty in to 1 and 0
y_pred = ifelse(prob_pred > 0.5, 1, 0)
#making the confusion matrix
cm = table(test_set[, 3 y_pred)
cm
#  y_pred
#0  1
#0 57  7
#1 10 26
# here 57 and 26 are the correct predictions and 10 and 7 are the incorrect preiction



#Visualizing the training set result
#install.packages('ElemStatLearn')
#library(ElemStatLearn)
#set = training_set
#X1 =seq(min(set[, 1]) -1, max(set[, 1]) +1, by = 0.01)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
#BUild a grid with X1 and X2, take minimum of the value from training set -1 because we dont't want to squze the value 
#in the graph. and same for the maximum +1
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) #age column
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) #salary column
grid_set = expand.grid(X1, X2) # setting the grid and create a matrix
colnames(grid_set) = c('Age', 'EstimatedSalary') #this is actual matrix 
#now here useing classifier to predict the result of each of the pixel observationpoints
# imaginary pixel points
#predict function returns probabilities which one format in 1 and 0 result

prob_set = predict(classifier, type = 'response', newdata = grid_set) 
y_grid = ifelse(prob_set > 0.5, 1, 0) #all the predictions on the grid that return vector of prediction on all the points in the grid.
plot(set[, -3
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2)) # plot the graph 
#
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green', 'red'))

#if the prediction boundaary is straight line that because the logistic regression is
#linear classifier. we are using 2 dimensional because we hav etaken two dimensions
#Classifier seperate the data points into two different regions. as 0 and 1
#those incorrect predictions specifically facts that classifier is linear classifier 
#and the user are not linearly distrubuted. and if the users arelinearly distuributed then
#the all the 0 are in one side and all the 1 are in one side




















