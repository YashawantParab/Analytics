#Importing data
DietData <-  read.csv("DietDataset.csv",header=TRUE)
summary(DietData)
View(DietData)
DietData <- na.omit(DietData)
View(DietData)
DietData = DietData[, 5:7]
View(DietData)
DatafitAnova <-  aov(pre.weight ~ weight6weeks, data=DietData)

summary (DatafitAnova)
#Summary: F( 2,12) actual result F value is 38.9 an dthe p value is 0.000635 is ery small and
# small than 0.05 result is significant
# Tukey compares the various means in parallel
TukeyHSD(DatafitAnova)

###########################################################
### ONE WAY ANOVA    ABSATZZAHLEN (SALES)
###########################################################

#d <- read.spss("Absatzzahlen.sav",to.data.frame=TRUE)

DietData <-  read.csv("DietDataset.csv",header=TRUE)
summary(DietData)
View(DietData)
DietData <- na.omit(DietData)
View(DietData)


fit <-  lm(weight6weeks ~ pre.weight + Diet, data=DietData)
nd <- data.frame(Weight6weeks=c(7,10,13), 
                pre.weight= c(150,1000, 2000), 
                Diet = c(60,80,120))
nd$Prediction <-  predict(fit,newDietData= DietData)
nd


## Now categorize number of visits : 0 to 80, 81 - 100, 101 - 140

d$besuche2 <-  cut(d$besuche, breaks=c(0,80,100,140))
table(d$besuche2)

# R uses effect coding: the first level is included in the intercept,
# all other levels will be tested against the first level. 
# we can see, that level two and level 3 are significant. 

fit2 <-  lm(menge~preis+ausgaben+besuche2,data=d)
summary(fit2)

#check, whether there is some interaction between expenses and visits. 
# In this example, this is not the case. If this was the case, 
# then one would need to inspect the interaction 
head(model.matrix(fit2))
summary(lm(menge~ausgaben*besuche2,data=d))


# we take again the example of the sales figures compared to the number of shop visits
# first we run one way ANOVA on the visits (3 levels) compared to the sold amount
fit <- aov(menge ~ besuche2, data = d)
summary (fit)

# Tukey compares the various means in parallel
TukeyHSD(fit)

# what we see is that only the comparison between the first and the third level is significant



##########################################################
## Two Way Anova ### DRUG STUDY
##########################################################


#load data from drug dosis study

d <- read.csv("./data/Dosisstudie.csv")
d$Dosis <- factor(d$Dosis, levels=c("Placebo","einfach", "doppelt"))
str(d)

# run 2 way ANOVA

fit <- aov(Score ~ Dosis*Geschlecht, data=d)
summary(fit)
op <- par(mfrow=c(1,1))
interaction.plot(d$Dosis,d$Geschlecht,d$Score)

TukeyHSD(fit)$"Dosis:Geschlecht"


####################YP####
head(warpbreaks)
summary(warpbreaks)
# use interaction effect
#aov is similar to lm(lineasr model)

model_1 <-aov (breaks ~ wool + tension, data=warpbreaks)
summary(model_1)
# p value is greater than 0.05 so not significant
# interaction effect
model_2 <-aov (breaks ~ wool + tension + wool:tension, data=warpbreaks)
summary(model_2)
#wool:tension (interaction effect) is significant
