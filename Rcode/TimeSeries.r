# Library
library(lattice)
library(gplots)
library(nparLD)
library(nlme)

# Data Load
pd <- read.csv("Data/alcohol1.csv", header = TRUE)

dim(pd)
str(pd)
head(pd)

# Sample data 
pd8 <- pd[pd$id %in% c(4,14,23,32,41,56,65,82),]

# Viz
xyplot(alcuse~age | id, data=pd8, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(-2, 5), as.table=T)

pd.male<-pd[pd$male==1,]
pd.ncoa<-pd[pd$coa==0,]

## interaction plot
interaction.plot(pd8$age, pd8$id, pd8$alcuse)

plotmeans(pd.ncoa$alcuse~ pd.ncoa$age, ylab="Members", main="Heterogeineity of Alc Use Over Time", data=pd.ncoa, lwd = 10, barwidth=5,  n.label=FALSE)

myplot <- nparLD(pd$alcuse ~ pd$age * as.factor(pd$coa), data = pd, subject = "id", description = FALSE)
plot(myplot)

# Model A

model.a <- lme(alcuse ~ 1, pd, random= ~1 |id)
summary(model.a)

VarCorr(model.a)
m<- VarCorr(model.a)
icc.a<-as.numeric(m[1,1]) / (as.numeric(m[1,1]) + as.numeric(m[2,1]))
icc.a
