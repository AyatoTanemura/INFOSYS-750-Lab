# Library----
library(tidyverse)

# Data Load ----

salary_df <- read.csv("Data/Employee_salary.csv", header = T) %>% 
  mutate(gender = as.factor(gender))

dim(df.salary)
str(df.salary)
head(df.salary)
# B ----
## Scatter plot ----

plot(salary_df$experience,salary_df$salary, xlab="Experience", ylab="Salary", main="Scatter Plot")

# Creating a dummy column
dummyGender <- (salary_df$gender == "female")*1

## add it to database
salary_df_D <- cbind(dummyGender, salary_df)
head(salary_df_D)

# C ----
## Models ----
fit0 <- lm(salary~experience+gender, salary_df)
summary(fit0)

# D----
## interaction effect----
fit1 <- lm(salary~experience+gender+experience:gender, salary_df)
summary(fit1)

# E ----
## Checking for two other models
fit2<-lm(salary~age+as.factor(gender), salary_df)
summary(fit2)

# F----
fit3<-lm(salary~experience+age+as.factor(gender), salary_df)
summary(fit3)

# G----
#correlation matrix
#For a better visualization you can use the mentioned package
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- salary_df_D[, c(1,3,4,5)]
chart.Correlation(my_data, histogram=TRUE, pch= 19)
cor(my_data)

#multicolinearity
install.packages("mctest")
library(mctest)
my_data1 <- salary_df_D[, c(1,3,5)]
# Individual Multicollinearity Diagnostic Measures
#Computes different measures of multicollinearity diagnostics such as 
# TOL,variance Inflation factor (VIF),Corrected VIF (CVIF), Klein's rule

imcdiag(x = my_data1, y = salary_df_D$salary)
#try imcdiag() function with model which has 3 independent variables which is "fit3"
# this below code works instead of the earlier imcdiag()

imcdiag(fit3) 












