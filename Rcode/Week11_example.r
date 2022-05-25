#Grunfeld's Investment Data
#https://www.stat.auckland.ac.nz/~wild/data/Rdatasets/doc/plm/Grunfeld.html
pd <- read.csv(file.choose(),header=TRUE)
names(pd)
dim(pd)

pd5<-pd[1:100,]
#randomly select few firms from the dataset
pd_random=pd[pd$firm %in% sample(unique(pd$firm),5),]

library(lattice)
xyplot(inv~year | firm, data=pd_random, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(0, 1200), as.table=T)

#Assumption checks - visualization (normality)
hist(pd$inv)
hist(pd$value)
hist(pd$capital)
#transformation is the replacement of a variable by a function of that variabl.
#for example, replacing a variable x by the square root of x or the logarithm of x.

#log transformations- is a strong transformation with a major effect on distribution shape. It is commonly used for reducing right skewness.
# It can not be applied to zero or negative values.
hist(log(pd$inv+1))
hist(log(pd$value+1))
hist(log(pd$capital+1))


#Model A
library(nlme)

model.a <- lme(log(inv+1)~ 1, pd, random= ~1 |firm)
summary(model.a)
VarCorr(model.a)


#Model B
model.b <- lme(log(inv+1) ~ I(year-1935) , data=pd, random= ~ I(year-1935) | firm, method="ML")
summary(model.b)
VarCorr(model.b)

#Covariance
getVarCov(model.b)

#Model C
model.c <- lme(log(inv+1) ~ log(capital+1)*I(year-1935) , data=pd, random= ~ I(year-1935)| firm, method="ML")
summary(model.c)
VarCorr(model.c)

#Model D
model.d <- lme(log(inv+1) ~ log(capital+1)*I(year-1935) + log(value+1)*I(year-1935)  , data=pd, random= ~ I(year-1935)| firm, method="ML")
summary(model.d)


#Model E
model.e <- lme(log(inv+1) ~ log(capital+1) + log(value+1)*I(year-1935)  , data=pd, random= ~ I(year-1935)| firm, method="ML")
summary(model.e)


#Model F
model.f <- lme(log(inv+1) ~ log(capital+1)*I(year-1935) + log(value+1)  , data=pd, random= ~ I(year-1935)| firm, method="ML")
summary(model.f)




