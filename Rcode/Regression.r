# Data Load----

df.sales <- read.csv("Data/WeeklyBeerSales.csv", header = TRUE)

dim(df.sales)
head(df.sales)
str(df.sales)

# Discriptive statistic ----
summary(df.sales)

# The average price per case was smaller for the larger carton size.
# Sales Volume was greater for the smaller carton size.

# Visualizing Data----

plot(df.sales$Week,df.sales$PRICE.18PK,xlab="Week",ylab="Price.18PK")
lines(df.sales$PRICE.18PK)

plot(df.sales$Week,df.sales$CASES.18PK,xlab="Week",ylab="Case.18PK")
lines(df.sales$CASES.18PK)

plot(df.sales$PRICE.18PK,df.sales$CASES.18PK, xlab="Price 18 Pack", ylab="Case 18 Pack", main="Scatter Plot")

# Regression model----

fitModel<-lm(CASES.18PK~PRICE.18PK, data=df.sales)

## adding regression line ----

abline(fitModel, col = "red")

# Interpret the Result ----

## regression analysis results ----

regResult<-summary(fitModel)
regResult

### Impact of the Coefficient ----

# Estimate regression equation: Predicted C18PK = 1812.184 - 93.007 * P18PK
# The model predicts that 93 fewer cases with 18-pk will be sold per $1 increas int he price per case
# The intercept in a regression model is rarely a number with any direct meaning

### Significance of the Coefficient ----

# our sample provides evidence that the price of the 18pk has a significant impact on the number of 18pk sold at the 0.01 level of significance.

### Performance of the model----

# The greater the R2, the greater the explanatory power of the regression model.
# our model explains 75% (adj R2 = 0.7457) of the variance observed in the sales of the 18pk.

## ANOVA ----
summary.aov(fitModel)

### Significance of the model----

# Statistical significance of the F ratio is attained when the reported p-vlue is less than a given significance level.
# All the independent variables int he models are jointly significant at the level of 0.01 of significance.

# Analyse Residual Plots----

## Residual vs Predicted ----
residualData<-resid(fitModel)
predicted<-predict(fitModel)
plot(predicted,residualData, xlab="Predicted Values", ylab="Residuals", col="red")

# the model makes bigger errors when bigger predictions
# An assumption in regression models is that "Error should have the same variance at all points, regardless of the values of the indepedent variables"
# This is evidence of heteroscedasticity
## error that do not have same variance for all levels of the predictions

## Residual plot----
plot(df.sales$Week,residualData)
abline(0, 0, col="red")

# The plot is specific to time series and shows that nearly all of the model's largest errors occured int he second half of the year.
# The reason for this is clear: most of the price manipulation and most of the spikes in sales occured in the second half of the year.

# The bottom line is that:
# the relationship between beer sales and beer price is evidently not linear over such a wide range in prices
# the model violated the homoscedasticity and normal-distribution assumptions for the errors.

# Not all models with high R2 value are good ones

# Transforming Existing Variables----



















