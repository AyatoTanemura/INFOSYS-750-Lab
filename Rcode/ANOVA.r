# Package Install ----
install.packages("nortest")
install.packages("lawstat")

# library ----
library("nortest")
library("lawstat")
library("PMCMRplus")
library("gplots")

# Load Data ----
 df <- read.csv("Data/ANOVA.csv", header = TRUE)

## data check----
 head(df)

#Normality Visualization
boxplot(df$x6)

boxplot(df$x6,
        main = "Box plot for Product Quality",
        xlab = "",
        ylab = "Product Quality",
        col = "red")

boxplot(x6 ~ x1, data = df,
        xlab = "customer type", ylab = "product quality",
        main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
        , names=c("<1 year","1-5 years",">5 years") )

boxplot(x6 ~ x1, data = df,
        xlab = "customer type", ylab = "product quality",
        main = "compare", col = c("red", "blue", "green")
        , names=c("<1 year","1-5 years",">5 years") )

hist(df$x6)

hist(df[df$x1==1, ]$x6)
hist(df[df$x1==2, ]$x6)
hist(df[df$x1==3, ]$x6)

# SW test ----
shapiro.test(df$x6)

#KS test ----
# for large dataset
lillie.test(df$x6)
lillie.test(df[df$x1 == 1,]$x6)

# Homogeneity of the variance ----
# cheking that all comparison groups have the same variance
# there are three versions of the Levene's test
  # use of mean (standard way to median above)
  # use of median (replace mean by median above)
  # use of 10% trimed mean (replace mean by 10% trimed mean above)

levene.test(df$x6, df$x1, location = c('mean'))
levene.test(df$x6, df$x1, location = c('median'))
levene.test(df$x6, df$x1, location = c('trim.mean'))

str(df)

# One-way ANOVA ----
fit <- aov(x6 ~ as.factor(x1), data = df)
summary(fit)

## Multiple comparison table ----
## Tukey
TukeyHSD(fit)
library("PMCMRplus")
summary(T2 <- tamhaneT2Test(df$x6, as.factor(df$x1)))








