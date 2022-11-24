## Header ####
## Assignment C7081 Statistical Analysis for Data Science ####

## Student ID <- 22358100 #####

## Choosing a new directory ####
setwd("D:/Datasets/1")

## Contents ####
## 01.Loading a new dataset
## 02.Understanding the Dataset
## 03.Basic commands
## 04.Basic Graphics
## 05.Simple linear regression
## 06.Multiple regression
## 07.Logistic regression
## 08.Linear Discriminate Analysis
## 09.Decision Trees
## 10.Conclusion

## 01. Loading dataset 1 - "new" Dataset ####
library(readxl)
new <- read_excel("new.xls")
View(new)

## 02. Understanding the Dataset ####

## Dataset 1: "new" - this dataset is about the Lung capacity(closing capacity) of the person #
# About this data, dataset is taken from Kaggle 
# https://www.kaggle.com/datasets/radhakrishna4/lung-capacity


## 03. Basic commands ####

length(new)

# the length of the dataset, here in the dataset we have 6 columns so the length is shown as 6 ##

ls(new)

# ls() function gives us the list of objects that is the columns in the dataset ##

# we calculate the mean, variance, square root, standard deviation of the Lungcapacity column in the dataset

mean(new$`LungCap(cc)`)

var(new$`LungCap(cc)`)

sqrt(new$`LungCap(cc)`)

sd(new$`LungCap(cc)`)

## 04. Basic Graphics ####

# plot() ##

y <- (new$`LungCap(cc)`)

x <- (new$`Height(inches)`)

plot(x,y)

# histogram of the data
 
hist(x)

# adding color for the histogram ##

hist(y, col = 2)

# both the x and y are showing gaussian distribution ##

## summary ##

# Summary function gives the numerical summary of all the variables in our dataset ##

summary(new)

# we can get summary of single variable ## 

summary(new$`LungCap(cc)`)

## 05.Simple linear regression ####

# Simple linear regression is useful for examining the relationship between 2 numeric values.

# fit a simple linear regression model using Age as Independent variable (X) variable #

# y is the dependent variable that is lung capacity # 

cor(`Age( years)`, `LungCap(cc)`)

# correlation of 0.819 shows there is a positive and linear association between Age and Lungcapacity

lm.fit <- lm(`LungCap(cc)`~`Age( years)`, data= new)
attach(new)
lm.fit <- lm(`LungCap(cc)`~ `Age( years)`)

lm.fit

# get a summary of this model

summary(lm.fit)

## Residuals:
# The Median is close to 0, so we can say our model is not skewed
# The residuals are symmetrically distributed as the Min and Max as well as 1Q and 3Q are close in magnitude.
# In the summary we got R-squared value of 0.6719 which tells us that approximately 67% of variation in Lung capacity can be explained by the Age factor
# The F-statistic and P-value for an overall test of significance of our model, this tests the Null Hypothesis that all the model coefficients are 0 ; here the slope for Age is 0.
# The residual standard error is the gives an idea of how far observed Lung capacity (Y values) are from the predicted or fitted Lung capacity ( the Y-hats), this gives us an idea of a typical sized residual or error.
# The intercept 1.14686 is the estimated mean Y value when all X values are 0.


names(lm.fit)

coef(lm.fit)

# the predict function used to produce confidence intervals and prediction intervals for the prediction of Lungcapacity for a given value of Age

predict(lm.fit, data.frame( `Age( years)` = (c(5, 10, 15) )), interval = "confidence")

predict(lm.fit, data.frame( `Age( years)` = (c(5, 10, 15) )), interval = "prediction")

# we will now plot Age and LungCap(cc) along with the least squares regression line using the plot() and abline() functions.

plot(`Age( years)` , `LungCap(cc)`)
abline(lm.fit)

# this gives us the visual evidence for linearity in the relationship between Age and LungCap(cc) 

plot(`Age( years)` , `LungCap(cc)`)

abline(lm.fit, lwd =3, col = "Gold")

## Diagnostics plots

par(mfrow = c(2, 2))
plot(lm.fit)

#Linearity is clearly seen in the Diagnostic plots as the red line is flat.
#In the Q-Q plot the residuals are in a diagonal line which tells us the Y values are normally dstributed


#The function rstudent() will return the studentized residuals, and we can use this function to plot the residuals against the ﬁtted values.

# The function rstudent() will return the studentized residuals, and we can use this function to plot the residuals against the ﬁtted values.

par(mfrow = c(1, 3))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(residuals(lm.fit), rstudent(lm.fit))

# On the basis of the residual plots, we can say the data is linear.

par(mfrow = c(1, 1))
plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit)) 

#The which.max() function identiﬁes the index of the largest element of a vector. In this case, it tells us which observation has the largest leverage statistic.

## Predicting the Lung Capacity based on the variable Age, Based on the summary we create an equation

# Based on the summary we create an equation with the intercept and coefficient values.

Y <- (1.14686) + (0.54485*20)

Y

# we took the Intercept and assumed the age of the individual as 20 and predicted the Lung capacity of 12.04386cc


## 06.Multiple regression ####

# Here, I used Age and height as explanatory X variables

lm.fit2 <- lm(`LungCap(cc)` ~ `Age( years)`+ `Height(inches)` , data = new)
summary(lm.fit2)

# by summary, the Multiple R squared value, we can say that,approximately 84% of variation in Lung capacity can be explained by Age and Height in this model
# Null hypothesis is 0, the slope for age and height is 0 by F-statistic and p-value
# residual standard error gives an idea of how far observed lung capacity Y values are from the predicted lung capacity Y Hats.
# intercept -11.747 is the estimated mean y value when all X values are 0, that is the estimated mean lung capacity for someone with age and height of 0 value
# slope for Age 0.126 is the effect of age on lung capacity adjusting or controlling for height, so with one year increase in Age an increase of 0.126 in lung capacity adjusting or controlling for height, hypothesis test for the slope for Age = 0
# slope for height is 0.278 is the estimated effect of height on lung capacity adjusting for age, the test for hypothesis, slope is 0

## correlation between age and height in Pearsons method
cor(`Age( years)`, `Height(inches)` , method = "pearson")

#we can see age and height are very highly correlated, it shows we should not diretly interpret the slope as the effect of Age on Lung Capacity adjusting for height
# the high value of correlation between age and height shows that these two effects are somewhat bounded together.

# y = a + b1x1 + b2x2 +...+ bnxn 

# Predicting the lung capacity based on this model

y <- (-11.747065) + (0.126368*22) + (0.278432*90)

y


# Here we took the intercepts and assumed the age as 20 years and height as 90 inches and predicted the lung capacity 16.09191 using the intercepts

# Anova

lm.fit <- lm(`LungCap(cc)`~ `Age( years)`)
anova(lm.fit, lm.fit2)

# The anova() function performs a hypothesis test comparing the two models.
# the F-statistic value of 786.84 and the associated P value is 0,this gives clear evidence that model 2 is superior yo model 1

# Diagnostic plots

par(mfrow = c(2, 2))
plot(lm.fit2)

par(mfrow = c(1, 1)).

# for non-linearity in the relationship between Lung capacity and Height, we have these residual plots

## 07.Logistic regression ####

glm.fit <- glm(`LungCap(cc)`~., data = new)

glm.fit

summary(glm.fit)

# In summary it shows that if the Gender is male and Smoking, age and height are significant in impacting the lungcapacity in this regression model


## 08. Linear Discriminant Analysis (LDA) ####

library(MASS)

lda.fit <- lda(Smoke ~ `Age( years)`, data = new)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit, new)
names(lda.pred)

# it shows that 89% of the individuals are not smoking and the rest 10% are smoking in our dataset.
# in the group it shows the age below 12.03 are not smoking and the age above 14.77 are smoking in our given dataset

lda.pred = predict(lda.fit, new)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Smoke)

#out of 725 total individuals, 648 are non smoking and 77 are smoking

mean(lda.class == Smoke)

# we calculate the mean that is 0.8937

sum( lda.pred$posterior[, 1] >= .5)

# Applying a 50% threshold to the posterior probabilities allows us to recreate the predictions contained in lda.pred$class.

sum( lda.pred$posterior[, 1] < .5)

lda.pred$posterior[1:20, 1]

lda.class[1:20] 

sum(lda.pred$posterior[ , 1] > .9)

# by this prediction there is a probability of increase in the number of smokers by the age can be predicted.


## 09. Decision trees ####

library(rpart)
library(rpart.plot)

# Load the diabetes dataset 

# Dataset 2: "diabetes" - this dataset is about the outcome of diabetes based on different functions #
# About this data, dataset is taken from Kaggle
# https://www.kaggle.com/datasets/akshaydattatraykhare/diabetes-dataset

diabetes <- read.csv("D:/Datasets/maybe/diabetes.csv")

summary(diabetes)

str(diabetes)

# Classification tree model

diabetes_model <- rpart(formula = diabetes$Outcome~. , data = diabetes, method = "class")

## display the decision tree

rpart.plot(x= diabetes_model, yesno= 2, type = 0, extra= 0)

# In the decision tree plot we can see, that if Glucose is more than 128, if it is yes the tree branches towards age and bmi, and gives the outcome of the diabetes.
