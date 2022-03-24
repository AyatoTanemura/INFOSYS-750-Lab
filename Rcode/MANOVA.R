# Manova
# Load Data ----
x <- read.csv("Data/MANOVA-1.csv", header = TRUE)

# Viewing Data
head(x)
names(x)

boxplot(x6 ~ x1, data = x,
        xlab = "customer type", ylab = "product quality",
        main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
        , names=c("<1 year","1-5 years",">5 years") )
boxplot(x7 ~ x1, data = x,
        xlab = "customer type", ylab = "E-Commerce",
        main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
        , names=c("<1 year","1-5 years",">5 years") )

shapiro.test(x$x6)
shapiro.test(x$x7)

shapiro.test(x[x$x1 == 1,]$x6)
shapiro.test(x[x$x1 == 2,]$x6)
shapiro.test(x[x$x1 == 3,]$x6)

hist(x$x6)
hist(x$x7)

hist(x[x$x1 == 1,]$x6)
hist(x[x$x1 == 2,]$x6)
hist(x[x$x1 == 3,]$x6)

qqnorm(x$x6)
qqline(x$x6)
hist(x$x6)
plot(density(x$x6))
