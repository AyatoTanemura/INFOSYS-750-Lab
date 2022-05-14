pd<-read.csv(file.choose(),header=TRUE)

View(pd)

pd8<-pd[pd$id %in% c(4, 14, 23, 32, 41, 56, 65, 82), ]
library(lattice)
xyplot(alcuse~age | id, data=pd8, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(-2, 5), as.table=T)

pd.male<-pd[pd$male==1,]
pd.ncoa<-pd[pd$coa==0,]
## Interaction plots
interaction.plot(pd8$age, pd8$id, pd8$alcuse) 
## 
library(gplots)
plotmeans(pd.ncoa$alcuse~ pd.ncoa$age, ylab="Members", main="Heterogeineity of Alc Use Over Time", data=pd.ncoa, lwd = 10, barwidth=5,  n.label=FALSE)

#####
library("nparLD")

myplot <- nparLD(pd$alcuse ~ pd$age * as.factor(pd$coa), data = pd, subject = "id", description = FALSE)

plot(myplot)

#Model A
library(nlme)

model.a <- lme(alcuse ~ 1, pd, random= ~1 |id)
summary(model.a)

#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/
VarCorr(model.a)
m<- VarCorr(model.a)
icc.a<-as.numeric(m[1,1]) / (as.numeric(m[1,1]) + as.numeric(m[2,1]))


###### More excercises on tolerance dataset
tolerance<-read.csv(file.choose(), header=T)
View(tolerance)


cor(tolerance[ , 2:6])
?cor

xyplot(tolerance ~ age | id, data=tolerance, as.table=T)


xyplot(tolerance~age | id, data=tolerance,
       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
       xlab = "Age", ylab = "Tolerance",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") },
       ylim=c(0, 4), as.table=T)

xyplot(tolerance ~ age | id, data=tolerance, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(0, 4), as.table=T)
