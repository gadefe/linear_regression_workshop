# Import the Data set
LungCapData <- read.delim(file.choose(), header=T)

#Attach the data

attach(LungCapData)

#Check names
names(LungCapData)

#Model the relationship between Age and Lung Capacity
#Lung Capacity is the outcome or dependent (Y) variable


#Check the type of variable for Age and LungCap

class (Age)

class (LungCap)

# Show the scatterplot of Age (X), LungCap (Y)

plot(Age, LungCap, main = "Scatterplot")

# Calculate the correlation using pearson's

cor(Age, LungCap)

# Create a model to predict LungCap using Age

model1 <- lm(LungCap ~ Age)

summary (model1)

#attributes of the model

attributes(model1)


sum1<- summary(model1)

attributes(sum1)

sum1$r.squared

#coefficients
model1$coefficients

# Draw the regression line

abline(model1, col=2, lw=3)

# Confidence Interval
confint(model1)

confint(model1, level=0.99)

summary (model1)
#anova table for f-test

anova(model1)


# square root of residuals mean square = residual SE

sqrt(2.3)

## Check the validity of the linear regression assumption
#linear model are never perfectly met, we must check for 
#reasonable assumption

#1- The Y-values(or the errors, "e") are independent
#2- The Y-values can be expressed as a linear function of the X variable
#3- Variation of observations around the regression line (the residual SE)
#   is constant (homoscedasticity)
#4- For given value of X, Y values (or the error) are Normally distributed

#1- knowledge of study design and data collection
#2-4 check by examining the reduals or errors

# R built in regression diagnostic plot

plot(model1)

# plot tile view

par(mfrow=c(1,1))

plot(model1)


#plot the cook's distance score
plot (model1, which=c(1,2,3,6))





