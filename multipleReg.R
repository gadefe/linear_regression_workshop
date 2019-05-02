#import dataset

WeightLossData <- read.delim(file.choose(), header=T)
attach(WeightLossData)

#Check names

names(WeightLossData)

# check variable types

str (WeightLossData)

# Draw scatter plot

# plot(height, weight, main = "scatterplot")

pairs(WeightLossData)

#create a linear association model between weigthloss and predictors

mod1 <- lm(WeightLoss ~ Exercise+LowCarbDiet+Stimulant)
summary (mod1)


#The model is significant. There is a linear association between WeightLoss and at least one predictor
#Exercise and lowCarbDiet are significant. The use of stimulant is not significant.
#Fit a regression model without the stimulant.

mod2 <- lm(WeightLoss~ Exercise+LowCarbDiet)
summary (mod2)


#model checking and regression diagnostics

#create a correlation matrix

round(cor(WeightLossData),2)


#checking for collinearity  
library(car)
vif (mod2)

#checking cook's distance measure

par(mfrow=c(2,2))
plot (mod2, which=c(2,4))

# Shapiro-Wilk Ttest for residuals normality of distribution

par(mfrow=c(2,2))
plot (mod2, which=c(1,2,3,4))

hist(residuals(mod2))
boxplot(residuals(mod2))
shapiro.test(residuals(mod2))


# Levene's Test for homogeneity of vaiance


weight.res <- residuals(mod2)
WeightLossData$res<-weight.res
WeightLossData$absres<-abs(weight.res)
WeightLossData$res2<-weight.res^2

WeightLossData$grp<-c(rep("1",12),rep("2",12))


head(WeightLossData)

View(WeightLossData)

leveneTest(res2~as.factor(grp), data=WeightLossData, center=mean)


#one way anova

analysis <-lm(res2~as.factor(grp), data=WeightLossData)
anova(analysis)




#Create the final regression model

mod2 <- lm(WeightLoss~ Exercise+LowCarbDiet)
summary (mod2)


#Interpretation
#Exercise and Low Carb Diet can explain about 86% of the variation in Weigth Loss
#The model significantly fits the data
#Exercise and Low Carb Diet significantly affect Weight Loss


