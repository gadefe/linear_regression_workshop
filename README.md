# Data Analytics Linear Regression
Simple linear regression using R programming

## Regression
Definition. Regression is a statistical tool that utilizes the relation
between two or more quantitative variables so that one variable
can be predicted from the other(s)

## Two types of Variables
## 1. Dependent variable
- response variable, outcome
- variable we want to predict
## 2. Independent variable
- explanatory variable, predictor variable, regressor
- the variable(s) used to predict the value of the dependent variable.

## Linear Regression parameters
### R squared
- R-squared is a statistical measure that represents the proportion of the variance for a dependent variable that's explained by an independent variable or variables in a regression model.
### p-value
- A p-value directly measures how well the samples agrees with Ho. The smaller the p-value, the stronger is the
  evidence for rejecting Ho.


Example of Hypothesis
- Advertising expendetures predict quarterly sales.
- Number of dependents predicts employee prescription drug expenses.
- Apartment size predicts montnly rent.
- Number of dinners predict business lunch expense.
- Assembly line speed predicts number of product defects.

More specific form
- How much extra sales will be generated, on average, by a 1 million increase in advertising expendetures?
  What would expected sales be with no advertising?
  
- How much do prescription drug cost per employee rise, on average, with each extra dependent?
  What would be the expected cost if the employee had no dependents?
  
- How much extra rent, on average, is paid per extra square foot?

- How much extra luncheon cost, on average, is generated by each additional members of the group?
  How much could be saved by restricting lucheon groups to three person?
  
- If the assembly line speed is increase by 20 units per hour, what would happend to the mean number of product defects?
  
 
## Interpreting a fitted regression

### Sales = 268 + 7.37 Ads
- Each extra 1 million of advertising will generate 7.37 million of sales on average.
  The firm would average 268 million of sales with zero advertising. However, the intercept may not be meaningful beacause Ads=0 may be outside the observed data.


### DrugCost = 410 + 550 Dependents
- Each extra dependents raises the mean annual prescription drug cost by 550 pesos. An employee with zero dependents averages 410 pesos in prescription drugs.

### Rent = 150 + 1.05 SqFt
- Each extra square foot adds 1.05 pesos to monthly apartment rent. The intercept is not meaningful because no apartment can have SqFt = 0.

### Cost = 15.22 + 19.96 Persons
- Each additional dinner increase the mean dinner cost 19.96 pesos. The intercept in not meaningful because Persons = 0 would not be observable.

### Defect = 3.2 + 0.045 Speed
- Each unit increase in assembly line speed adds an average of 0.045 defects per million. The intercept is not meaningful since zero assembly line speed implies no production at all.

## Prediction Using Regression
### Sales = 268 + 7.37 Ads
- If the firm spends 10 million on advertising, its expected sales would be 341.7 million, that is,
- Sales = 268 + 7.37(10) = 341.7.

### DrugCost = 410 + 550 Dependents
- If an employee has four dependents, the expected annual drug cost would be 2,610, that is
- DrugCost = 410 + 550(4) = 2,610.

### Rent = 150 + 1.05 SqFt
- The expected rent on an 800 square foot apartment is 990, that is
-  Rent = 150 + 1.05 (800) = 990

### Cost = 15.22 + 19.96 Persons
- The expected cost of dinner for two couples would be 95.06 pesos, that is,
- Cost = 15.22 + 19.96 (4) = 95.06

### Defect = 3.2 + 0.045 Speed
- If 100 units per hour are produced, the expected defect rate is 7.7 defects per million, that is,
- Defect = 3.2 + 0.045 (100) = 7.7


## Simple Linear Regression using R


### Consider: Heights and Weights of Seven People

![](img/dataset.jpg)

Given a person’s height, can we
predict the person’s weight?

How can we best describe the
relationship between height and
weight?


### Load the data in R using the following command.

![](img/loaddata.jpg)

- Step1: Draw the scatter plot diagram

![](img/scatterplot.jpg)


- Step2: Test for significance using correlation

![](img/cortest.jpg)


### interpretation of the test score

The correlation test score shows positive association between Height and the Weight variable. As height increases, weight tends to increase. Regarding the strength of the relationship, the test scores shows extremely high relationship where the data points tightly hug a line. Its an entirely amorphous blob with a very high correlation. That description matches an extremely high correlation coefficient of 0.977.

For the hypothesis test, our p-value equals 0.0001458. This p-value is less than any reasonable significance level. Consequently, we can reject the null hypothesis and conclude that the relationship is statistically significant. The sample data support the notion that the relationship between height and weight exists in the population. 




- Step3: Draw the fitted regression line

![](img/fittedline.jpg)

- Step4: Show the statistical summary of the regression model


![](img/summary2.jpg)

## interpretation of the test score

### The R squared = 95.53% 
- Height explains 95.53% of the total variation in Weight

### The p-value = 0.0001458
- The model is significant (p<0.05), therefore the linear model significantly fits the data

###  y-intercept = - 439.693 and the slope = 8.797
- If the height increase by 1 inches, the average weight increase by 8.797 pounds.
- The y-intercept and the slope are significant (p < 0.05). Height significantly predicts weight

## Estimated Regression Equation

### Weight = - 439 + 8.797 Height
- For every unit increase in Height, weight increases on the average by 8.787 pounds. The intercept in not meaningful because Height = 0 would not be observable.
- Height explains 95% of the total variation in Weight

- The expected Weight of a person with 83 inches in Height is 290 pounds, that is
- Weight = -439 + 8.787 (83) = 290


## Multiple Linear Regression 

Multiple linear regression (MLR), also known simply as multiple regression, is a statistical technique that uses several explanatory variables to predict the outcome of a response variable. The goal of multiple linear regression (MLR) is to model the linear relationship between the explanatory (independent) variables and response (dependent) variable.

Let’s suppose that we’ve got three variables that we’re interested in; perhaps we want to use Exercise, Stimulant and Low Carb Diet to predict the WeightLoss variable.

### Load the data in R using the following command.

![](img/LoadWeightLossData.jpg)

- Step1: Draw the pair scatter plot diagram

![](img/PairScatterPlot.jpg)

- Step2: Test for association using pair correlation

![](img/PairCorrelation.jpg)

- Step3: Create a linear regression model

![](img/mod1MultiRegSummary.jpg)

### interpretation of the regression model1

The model shows a good fit (R-squared = 0.8752) and is significant overall (p-value = 3.2e-09), it shows linear association between WeightLoss and at least one predictor variable. The Exercise and Low Carb Diet have a significant effect on WeightLoss at alfa = 0.05 with a value of (p = 4.98e-07) and (p = 0.00887) respectively. Regarding the Stimulant coeffecient (p = 0.10207) suggest is not statistically significant.


- Step4: Generate a linear regression model without the Stimulant

![](img/mod2MultiRegSummary.jpg)

- Step5: Check for collinearity using variance inflation factor (VIF) < 10.

![](img/vifTest.jpg)

- Step5: Check for influential observation using Cook's Distane Measure < 1.

![](img/cooksdist.jpg)

- Step6: Check for normality using Shapiro_Wilk Test

![](img/shapirotest.jpg)

- Step7: Check for equality of variance using Levene's Test

![](img/levenetest.jpg)

- Step4: Generate the final linear regression model

![](img/mod2MultiRegSummary.jpg)


### interpretation of the final regression model

The model shows a good fit (R-squared = 0.8752) and is significant overall (p-value = 3.2e-09), it shows linear association between WeightLoss and at least one predictor variable. The Exercise and Low Carb Diet have a significant effect on WeightLoss at alfa = 0.05 with a value of (p = 4.98e-07) and (p = 0.00887) respectively. Regarding the Stimulant coeffecient (p = 0.10207) suggest is not statistically significant.










 
