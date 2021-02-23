####################################################
# PLSC 503 -- Spring 2021: Code for week six
# (outliers / influence + simultaneity/endogeneity)
####################################################
# Packages:
# install.packages("gvlma") # <- uncomment as needed
library(gvlma)
library(RCurl)
library(MASS)
library(sem) # <-- install if needed
library(car)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places
###################################################
# "Flintstones" data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/flintstones.csv")
flintstones<-read.csv(text=temp, header=TRUE)
rm(temp)

# No Barney OR Dino:
summary(lm(Y~X,data=subset(flintstones,name!="Dino" & name!="Barney")))

# No Barney (Dino included):
summary(lm(Y~X,data=subset(flintstones,name!="Barney")))

################
# Dahl data...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/LittleDahl.csv")
LittleDahl<-read.csv(text=temp, header=TRUE)
rm(temp)

library(car)
with(LittleDahl, scatterplotMatrix(~age+tenure+unified+nulls))

Fit<-with(LittleDahl, lm(nulls~age+tenure+unified))
summary(Fit)

FitResid<-with(LittleDahl, (nulls - predict(Fit))) # residuals
FitStandard<-rstandard(Fit) # standardized residuals
FitStudent<-rstudent(Fit) # studentized residuals
FitCooksD<-cooks.distance(Fit) # Cook’s D
FitDFBeta<-dfbeta(Fit) # DFBeta
FitDFBetaS<-dfbetas(Fit) # DFBetaS
FitCOVRATIO<-covratio(Fit) # COVRATIOs

FitStudent[74]
LittleDahl$Congress74<-rep(0,length=104)
LittleDahl$Congress74[74]<-1
summary(with(LittleDahl, lm(nulls~age+tenure+unified+Congress74)))

influencePlot(Fit,id.n=4,labels=LittleDahl$Congress,id.cex=0.8,
              id.col="red",xlab="Leverage")

dfbetasPlots(Fit,id.n=5,id.col="red",main="",pch=19)

plot(FitCOVRATIO~names(FitCOVRATIO),pch=19,xlab="Congress",
     ylab="Value of COVRATIO")
abline(h=1,lty=2)

Outlier<-rep(0,104)
Outlier[74]<-1
Outlier[98]<-1
Outlier[104]<-1
DahlSmall<-LittleDahl[which(Outlier==0),]

summary(lm(nulls~age+tenure+unified,data=DahlSmall))

#############################################
# Africa example (for -glvma-):

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Africa)
rownames(Africa) <- Africa$cabbr

Fit <- with(Africa, 
            lm(adrate~gdppppd+muslperc+subsaharan+healthexp+
                 literacy+internalwar))
summary(Fit)

# What not to do:

library(gvlma)
Nope <- gvlma(Fit)
display.gvlmatests(Nope)

# Better:

pdf("DefaultLMPlots.pdf",10,8)
par(mfrow=c(2,3))
plot(Fit,which=c(1:6),labels.id=rownames(Africa))
dev.off()

# Unpacking that:
#
# #1: Residuals vs. fitted, same as:

pdf("ResidVsFitted.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Fit,which=1,labels.id=rownames(Africa))
dev.off()

# #2: QQ plot of residuals:

pdf("ResidQQPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=2,labels.id=rownames(Africa))
dev.off()

# #3: Scale-Location plot:

pdf("ScaleLocationPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=3,labels.id=rownames(Africa))
dev.off()

# #4: Cook's Distance (D):

pdf("CooksDPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=4,labels.id=rownames(Africa))
dev.off()


# #5: Residuals vs. Leverage:

pdf("ResidVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=5,labels.id=rownames(Africa))
dev.off()

# #6: Cook's D vs. Leverage:

pdf("CooksDVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=6,labels.id=rownames(Africa))
dev.off()

# Another useful plot:

ToPlot<-data.frame(Africa$adrate,Fit$fitted.values,
        Fit$residuals,Africa$gdppppd,Africa$muslperc,
        Africa$subsaharan,Africa$healthexp,Africa$literacy,
        Africa$internalwar)

pdf("UsefulPlot.pdf",8,7)
scatterplotMatrix(ToPlot)
dev.off()

################################################
# IV Estimation...
#
# Simulated example:

seed<-1337
set.seed(seed)

mu<-c(0,0,0) # <== X, Z, U
Sigma<-matrix(c(1,0.8,0.4,0.8,1,0,0.4,0,1),
              nrow=3,byrow=TRUE) # Cor(X,Y)=0.8, etc.
Vars<- mvrnorm(500,mu,Sigma)
colnames(Vars)<-c("X","Z","U")
Vars<-data.frame(Vars)

Vars$Y<- 1 + Vars$X + Vars$U

pdf("IVSimScatter.pdf",7,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(Vars,pch=20)
dev.off()

# OLS:

OLS<- lm(Y~X,data=Vars)
summary(OLS)

# 2SLS:

TSLS<-tsls(Y~I(X),data=Vars,instruments=~Z)
summary(TSLS)

##### "Real data" example... #####

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/IRData.csv")
IRData<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(IRData)

# OLS:

OLSWar<-lm(logdisputes~logtrade+contiguity+capratio,
           data=IRData)
summary(OLSWar)

# 2SLS:

TwoSLSWar<-tsls(logdisputes~contiguity+capratio+I(logtrade),
                instruments=~contiguity+capratio+IOs,
                data=IRData)
summary(TwoSLSWar)

# "By hand":

ITrade<-lm(logtrade~contiguity+IOs+capratio,
           data=IRData)
summary(ITrade)
IVWarByHand<-with(IRData,
                  lm(logdisputes~capratio+contiguity+
                       (ITrade$fitted.values)))
summary(IVWarByHand)

# Now, trade:

OLSTrade<-lm(logtrade~logdisputes+contiguity+IOs,
             data=IRData)
summary(OLSTrade)

TwoSLSTrade<-tsls(logtrade~contiguity+IOs+I(logdisputes),
                  instruments=~contiguity+capratio+IOs,
                  data=IRData)
summary(TwoSLSTrade)

# Plots of IVs:

IWar<-lm(logdisputes~contiguity+IOs+capratio,
         data=IRData)

# The good:

pdf("PrettyGoodInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRData, 
     plot(logtrade,ITrade$fitted.values,pch=20,col="black",
          xlab="Actual Values of ln(Trade)",
          ylab="Predicted Values of ln(Trade)"))
abline(a=0,b=1,lwd=2)
dev.off()

# ...and the bad:

pdf("CrappyInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRdata, 
     plot(logdisputes,IWar$fitted.values,pch=20,col="black",
          ylim=c(-1,3),xlab="Actual Values of ln(Disputes)",
          ylab="Predicted Values of ln(Disputes)"))
abline(a=0,b=1,lwd=2)
dev.off()
