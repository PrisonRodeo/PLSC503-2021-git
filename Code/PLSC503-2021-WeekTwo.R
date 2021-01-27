######################################
# Code for PLSC 503 Week Two
# (Spring 2021)
#
# File created 1/19/2017-ish
#
# File last modified 1/27/2021
######################################
# setwd() first:
#
# setwd("~/PLSC 503/Notes") # or whatever...
#
# Load relevant packages:

library(RCurl)
# install.packages("psych") <- install psych package, if necessary
library(psych)
# install.packages("stargazer") <- install if necessary
library(stargazer)

########################################
# "World's simplest regression"
#
# N = 3 plot...

df <- data.frame(X = c(1,2,3),
                 Y = c(1,1,7))
Xmean <- mean(df$X)
Ymean <- mean(df$Y)
fit3 <- with(df, lm(Y~X))
df$hats <- fit3$fitted.values
df$resid <- fit3$residuals

pdf("Nequals3-R.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,Y,pch=19,ylim=c(0,8),xlim=c(0.5,3.5)))
abline(h=Ymean)
with(df, segments(X,Y,X,hats,lwd=2,lty=2,col="red"))
segments(df$X,Ymean,df$X,df$hats,lwd=2,lty=3,col="green")
abline(fit3,lwd=3)
with(df, points(X,Y,pch=20))
legend("topleft",bty="n",lwd=c(3,2,2,1),lty=c(1,2,3,1),
       col=c("black","red","green","black"),
       legend=c("Regression line","RSS","MSS","Mean of Y"))
dev.off()

WSR<-lm(Y~X)
summary(WSR)

########################################
# Running infant mortality example...
#
# Read in data (from repo):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the data
rm(url)

# Summary statistics

with(Data, describe(infantmortalityperK))
with(Data, describe(DPTpct))

# Regression:

IMDPT<-lm(infantmortalityperK~DPTpct,data=Data,na.action=na.exclude)
summary.lm(IMDPT)

# Scatterplot:

pdf("IMDPT.pdf",6,6) # <- create PDF
with(Data, plot(DPTpct,infantmortalityperK,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Infant Mortality (Deaths per 1000 Births)"))
with(Data, text(DPTpct,infantmortalityperK,labels=WBcode))
with(Data, abline(v=mean(DPTpct,na.rm=TRUE),lty=2))
with(Data, abline(h=mean(infantmortalityperK,na.rm=TRUE),lty=2))
abline(IMDPT,lwd=3)
dev.off()

# ANOVA

anova(IMDPT)

# Residuals (u):

Data$IMDPTres <- with(Data, residuals(IMDPT))
describe(Data$IMDPTres)

# Residual density plot:

pdf("IMDPTResidualsDensity.pdf",6,6)
with(Data, plot(density(IMDPTres,na.rm=TRUE),
                main="Density Plot: Regression Residuals",
                xlab="Residual Value"))
abline(v=0,lty=2,lwd=2)
dev.off()

# Fitted Values:

Data$IMDPThat<-fitted.values(IMDPT)
describe(Data$IMDPThat)

# Densities plot:

pdf("IMDPTFittedDensity.pdf",6,6)
with(Data, plot(density(IMDPThat,na.rm=TRUE),
                main="Density Plot: Actual and Fitted Values",
                xlab="Values of Y"))
with(Data, lines(density(infantmortalityperK,na.rm=TRUE),
                lty=2,col="red"))
with(Data, abline(v=mean(infantmortalityperK,na.rm=TRUE),
                  lty=2,lwd=2))
dev.off()

# Correlations:

with(Data, cor(infantmortalityperK,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPTres,DPTpct,use="complete.obs"))
with(Data, cor(IMDPThat,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPThat,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,IMDPThat,use="complete.obs"))

# Plotting residuals vs. X:

pdf("IMDPTResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres,labels=WBcode))
abline(h=0,lwd=2)
dev.off()

# Squared residuals vs. X:

pdf("IMDPTSquaredResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres^2,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres^2,labels=WBcode))
dev.off()

###########################
# Inference:

# Estimated variance-covariance matrix:

vcov(IMDPT)

# Confidence intervals around the betas:

confint(IMDPT)

# Change the significance level...

confint(IMDPT,level=0.99)

# Predictions w/standard errors:

SEs<-predict(IMDPT,interval="confidence")
SEs

# Plot it!:

Sort<-order(IMdata$DPTpct)

pdf("IMDPT-CI.pdf",6,5)
plot(IMdata$DPTpct,IMdata$infantmortalityperK,
     xlab="DPT Immunization Percentage",
     ylab="Infant Mortality Per 1000 Births")
abline(IMDPT,lwd=3)
lines(sort(IMdata$DPTpct),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(IMdata$DPTpct),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

#############################
# Model fit and R-squared...

# Simulations:

seed <- 7222009
set.seed(seed)

X<-rnorm(250)
Y1<-5+2*X+rnorm(250,mean=0,sd=sqrt(0.2))
Y2<-5+2*X+rnorm(250,mean=0,sd=sqrt(20))
fit<-lm(Y1~X)
summary(fit)

pdf("TightLine-R.pdf",5,5)
plot(X,Y1,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,15))
abline(fit,lwd=3)
text(-1.5,12,labels="R-squared = 0.95")
dev.off()

fit2<-lm(Y2~X)
summary(fit2)

pdf("ScatteredLine-R.pdf",5,5)
plot(X,Y2,pch=20,xlab="X",ylab="Y",
     xlim=c(-2.5,2.5),ylim=c(-10,15))
abline(fit2,lwd=3)
text(-1.5,12,labels="R-squared = 0.20")
dev.off()

# R^2 = 0 plots:

seed<-7222009
set.seed(seed)

pdf("RSqZero.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
X<-(runif(100))*10
Yzero<-(runif(100))*10
Yquad<-30-((X-5)^2)+(2*runif(100))
Ystep<-ifelse(abs(X-5)>2.5,5+runif(100),1+runif(100))
Ytype<-rep(0:1,50)
Yvar<-ifelse(Ytype==1,X+(2*runif(50)),10-X+2*runif(50))
plot(Yzero~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yzero~X),lwd=3)
plot(Yquad~X,xlab="X", ylab="Y",pch=20)
abline(lm(Yquad~X),lwd=3)
plot(Ystep~X,xlab="X", ylab="Y",pch=20)
abline(lm(Ystep~X),lwd=3)
plot(Yvar[Ytype==0]~X[Ytype==0],xlab="X", ylab="Y",pch=20)
points(Yvar[Ytype==1]~X[Ytype==1],pch="o")
abline(lm(Yvar~X),lwd=3)
dev.off()

#######################################
# STUPID REGRESSION TRICKS
#######################################

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/africa2001.csv")
africa<-read.csv(text=temp, header=TRUE)
summary(africa)

fit<-with(africa, lm(adrate~muslperc))
summary(fit)

# First figure

SEs<-predict(fit,interval="confidence")
Sort<-order(africa$muslperc)

pdf("Notes/SRTFig1.pdf",6,5)
plot(africa$muslperc, africa$adrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16) 
abline(fit,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 10:

africa$muslplusten<-africa$muslperc+10
fit2<-with(africa, lm(adrate~muslplusten,data=africa))
summary(fit2)

SEs<-predict(fit2,interval="confidence")

pdf("Notes/SRTFig2.pdf",6,5)
plot(africa$muslplusten, africa$adrate, 
     xlab="Muslim Percentage of the Population + 10", 
     ylab="HIV Prevalence Rate",pch=16)
abline(fit2,lwd=3)
lines(sort(africa$muslplusten),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslplusten),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Multiply Y times -314:

africa$screwyrate<-africa$adrate*(-314)
fit3<-with(africa, lm(screwyrate~muslperc))
summary(fit3)

SEs<-predict(fit3,interval="confidence")

pdf("Notes/SRTFig3.pdf",6,5)
plot(africa$muslperc, africa$screwyrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate times -314",pch=16,
     ylim=c(-13000,1000))
abline(fit3,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Reversing the scales:

africa$nonmuslimpct <- 100 - africa$muslperc
africa$noninfected <- 100 - africa$adrate
fit4<-lm(noninfected~nonmuslimpct,data=africa)
summary(fit4)

SEs<-predict(fit4,interval="confidence")
Sort2 <- order(africa$nonmuslimpct)

pdf("Notes/SRTFig4.pdf",6,5)
plot(africa$nonmuslimpct, africa$noninfected, 
     xlab="Non-Muslim Percentage of the Population", 
     ylab="HIV Non-Prevalence Rate",pch=16,
     ylim=c(60,111))
abline(fit4,lwd=3)
lines(sort(africa$nonmuslimpct),SEs[Sort2,2],col="red",lwd=2,lty=2)
lines(sort(africa$nonmuslimpct),SEs[Sort2,3],col="red",lwd=2,lty=2)
dev.off()

# Centering X:

africa$muslcenter<-africa$muslperc - mean(africa$muslperc, na.rm=TRUE)
fit5<-lm(adrate~muslcenter,data=africa)
summary(fit5)
mean(africa$adrate)

SEs<-predict(fit5,interval="confidence")

pdf("Notes/SRTFig5.pdf",6,5)
plot(africa$muslcenter, africa$adrate, 
     xlab="Centered Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16,
     ylim=c(-10,40))
abline(fit5,lwd=3)
lines(sort(africa$muslcenter),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslcenter),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Rescaling X for interpretability:

fit6<-lm(adrate~population,data=africa)
summary(fit6)

africa$popmil<-africa$population / 1000000
fit7<-lm(adrate~popmil,data=africa)
summary(fit7)

# Dichotomous X and t-tests:

fit8<-lm(adrate~subsaharan,data=africa)
summary(fit8)

with(africa,
     t.test(adrate~subsaharan, var.equal=TRUE))

# Reporting:

fit<-lm(adrate~muslperc, data=africa)
summary.lm(fit)

# Easy LaTeX table using *stargazer*:

stargazer(fit,
          type="latex",
          title="OLS Regression Model of HIV/AIDS Rates in Africa, 2001",
          dep.var.caption="",
          dep.var.labels="Model I",
          digits=2,
          no.space=TRUE,
          intercept.top=TRUE,intercept.bottom=FALSE,
          covariate.labels=c("(Constant)","
                             Muslim Percentage of the Population"))

