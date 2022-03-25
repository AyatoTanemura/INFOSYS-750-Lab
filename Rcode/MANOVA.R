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

##https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
qqnorm(x$x7)
qqline(x$x7)
hist(x$x7)
plot(density(x$x7))

#Outliers detection and removal

outliersx6<-boxplot(x6 ~ x1, data = x,
                    xlab = "customer type", ylab = "product quality",
                    main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07"),
                    names=c("<1 year","1-5 years",">5 years") )$out
print(outliersx6)

out1<-x[x$x1==3 & x$x6==7.6,]
print(out1)
out2<-x[x$x1==1 & x$x6==9.5,]
print(out2)

outliersx7<-boxplot(x7 ~ x1, data = x,
                    xlab = "customer type", ylab = "E-Commerce",
                    main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
                    , names=c("<1 year","1-5 years",">5 years") )$out
print(outliersx7)

out3<-x[x$x1==2 & (x$x7==5.1 | x$x7==5.5),]
print(out3)
out4<-x[x$x1==1 & x$x7==5.6,]
print(out4)

outAll<- rbind(out1,out2, out3, out4)
print(outAll)
# 10 observations will be deleted from the dataset

x1<-x[-which(x$id %in% outAll$id),]

