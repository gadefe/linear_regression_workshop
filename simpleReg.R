#import dataset

WeightHeightData <- read.delim(file.choose(), header=T)
attach(WeightHeightData)

#Check names

names(WeightHeightData)

# check variable types

class (WeightHeightData$height)

# Draw scatter plot
plot(height, weight, main = "scatterplot")

#caculate the correlation using pearson's

cor.test(height,weight)

#Create linear model

mod <- lm (weight~ height)

summary (mod)

abline (mod, col=2, lw=2)


