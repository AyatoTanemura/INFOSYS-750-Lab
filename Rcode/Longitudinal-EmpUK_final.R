pd <- read.csv(file.choose(),header=TRUE)
names(pd)
pd10<-pd[1:35,]
library("lattice")
xyplot(emp~year | firm, data=pd10, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(0, 100), as.table=T)

#Model A
library(nlme)

model.a <- lme(emp~ 1, pd, random= ~1 |firm)
summary(model.a)


VarCorr(model.a)
mm=VarCorr(model.a)
ICC<-as.numeric(mm[1,1])/(as.numeric(mm[2,1])+as.numeric(mm[1,1]))
#I(year-1976)
a=I(pd$year-1976)

#Model B
model.b <- lme(emp ~ I(year-1976) , data=pd, random= ~ year | firm, method="ML")
summary(model.b)
mm1=VarCorr(model.b)



fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + I(pd$year[1:7]-1976)*fixef.b[[2]]
plot(pd$year[1:7], fit.b, ylim=c(0, 20), type="b", 
     ylab="predicted emp", xlab="year")   
title("Model B \n Unconditional growth model")

#Model C
model.c <- lme(emp ~ sector*I(year-1976) , data=pd, random= ~ year | firm, method="ML")
summary(model.c)
mm2=VarCorr(model.c)

#Pseudo R2
r2<- (as.numeric(mm1[1,1])-as.numeric(mm2[1,1]))/as.numeric(mm1[1,1])

#Model D
model.d <- lme(emp ~ sector*I(year-1976)+wage*I(year-1976) , data=pd, random= ~ year | firm, method="ML")
summary(model.d)
VarCorr(model.d)

#Model E
model.e <- lme(emp ~ sector+wage*I(year-1976) , data=pd, random= ~ year | firm, method="ML")
summary(model.e)
VarCorr(model.e)


#Standard errpr calcultion in r
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/


