library(ggplot2)
data(cars)
str(cars)
head(cars)
plot(speed ~dist, data = cars)
#calculate  correlation coefficient using cor function
cor(cars$speed, cars$dist)
#0.8068949 means strong, positive linear relationship


#Lm means linear model, lm(Dependent variable ~independent variable... means Y~X)
m1 <- lm(speed ~ dist, data = cars)
ls()
m1
#lm(formula = speed ~ dist, data = cars)

#Coefficients:
#  (Intercept)         dist  
#   8.2839 (b0)       0.1656(b1)

summary(m1)
# Summary explains: Residuals min and maximum values,........
#Coefficients : Intercepts(b0) and slope(b1)
# R squared and adjusted R squared
#F stats
#Degree of fredom
#p value

#slope
#lty means type of line, lwd: thickness of line
abline(m1, col= "red", lty = 1, lwd =2)
m1
#make a prediction
predict(m1, data.frame("dist"= 25), interval = "prediction")
# and the result will be the prediction with upper bound and lower bound 
