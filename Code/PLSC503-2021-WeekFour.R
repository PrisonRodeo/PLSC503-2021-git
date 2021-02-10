################################################
# PLSC 503 -- Spring 2021: Code for Week Four.
#
# Dichotomous predictors and transformations
# (ideally, towards linearity).
################################################
# setwd() here:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
# Packages:

library(RCurl)
library(car)

#####################################################
# Ugly dummy variable scatterplot:

set.seed(7222009)
X <- rbinom(100,1,0.5)
Y <- 10 + 10*X + (5*rnorm(100))
Dfit <- lm(Y~X)
Dhat <- predict(Dfit)

pdf("UglyDummyScatterplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
plot(X,Y,pch=20,xaxt="n")
axis(1,at=c(0,1))
points(X,Dhat,pch=4,col="red",cex=2)
abline(Dfit,lwd=2,col="red",lty=2)
legend("bottomright",legend="indicates E(Y|X=0/1)",
       bty="n",col="red",pch=4)
dev.off()

# Better boxplot:

pdf("DummyBoxplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
boxplot(Y~X)
points(X+1,Dhat,pch=4,col="red",cex=2)
abline(a=Dfit$coefficients[1]-Dfit$coefficients[2],
       b=Dfit$coefficients[2],
       lwd=2,col="red",lty=2)
legend("bottomright",legend="indicates E(Y|X=0/1)",
       bty="n",col="red",pch=4)
dev.off()

# SCOTUS (1953-1985) data...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/SCOTUS.csv")
SCOTUS<-read.csv(text=temp, header=TRUE)
summary(SCOTUS)

summary(SCOTUS)

SCOTUS$civil.econ<-SCOTUS$civlibs + SCOTUS$econs

SCOTUS$termdummies<-factor(SCOTUS$term)

fit1<-with(SCOTUS, lm(Namici~civlibs))
summary(fit1)
with(SCOTUS, t.test(Namici~civlibs))

SCOTUS$civlibeffect<-SCOTUS$civlibs
SCOTUS$civlibeffect[SCOTUS$civlibs==0]<-(-1)
fit2<-with(SCOTUS, lm(Namici~SCOTUS$civlibeffect))
summary(fit2)

fit3<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib))
summary(fit3)

fit4<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib+term))
summary(fit4)

fit5<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                     econs+constit+lctlib+as.factor(term)))
summary(fit5)

# Plot coefficients:

termbetas<-fit5$coefficients[8:39]
SE5<-sqrt(diag(vcov(fit5)))[8:39]
termUBs <- termbetas + 1.96*(SE5)
termLBs <- termbetas - 1.96*(SE5)
term<-seq(1954,1985)

pdf("TermBetasR.pdf",6,5)  
par(mar=c(4,4,2,2))
plot(term,termbetas, xlab="Term",ylab="Estimated Betas",
     pch=19,ylim=c(-0.5,2.1))
lines(term,termbetas,lwd=2)
segments(term,termLBs,term,termUBs,lwd=2,lty=2)
abline(h=0,lwd=2,lty=2,col="red")
dev.off()

#################################################
# Transformations...
#
# What difference? Non-Linearity plots:

set.seed(7222009)
X <- runif(200,-4,4)
Y <- exp(X)+runif(200,0,20)
fit <- lm(Y~X)

pdf("NonLinMisspec1.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(X,Y,pch=20)
abline(lm(Y~X),lwd=3)
segments(X,Y,X,fit$fitted.values,lwd=1,lty=2,col="red")
legend("topleft",bty="n",legend="Regression")
plot(X,fit$residuals,ylab="Residuals",pch=20)
abline(h=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Residuals vs. X")
dev.off()

pdf("NonLinMisspec2.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(fit$residuals),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
qqPlot(fit$residuals,xlab="Normal Quantiles",ylab="Residuals",
       pch=20)
dev.off()

########
# Simulations for non-normal residuals:

sims <- 1000
set.seed(7222009)
N <- 20  # small sample
NN <- 2000 # big sample...

OKBs <- numeric(sims)
BadBs <- numeric(sims)
OKTs <- numeric(sims)
BadTs <- numeric(sims)

for (i in 1:sims) {
  u <- rnorm(N,0,2) # mean zero, s.d = 2
  # Exponentiate:
  eu <- exp(u)
  eu <- eu-mean(eu) # new residuals are mean-zero
  eu <- (eu/sd(eu))*2 # and also sd = 2
  
  # Generate Ys:
  
  X <- runif(N,-4,4)
  Y1 <- 0 + 1*X + 1*u 
  Y2 <- 0 + 1*X + 1*eu # same Xs in both
  
  fit1 <- lm(Y1~X)
  fit2 <- lm(Y2~X)
  
  OKBs[i] <- fit1$coefficients[2]
  BadBs[i] <- fit2$coefficients[2]
  OKTs[i] <- summary(fit1)$coefficients[2,3]
  BadTs[i] <- summary(fit2)$coefficients[2,3]
} 

# Plots...

pdf("NonLinBadResids.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(u),main=" ",xlab="Residual Values",ylim=c(0,0.25))
abline(v=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals")
plot(density(eu),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals")
dev.off()

pdf("NonLinBsAreJustFine.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(OKBs),main=" ",xlab="Estimated Slope",ylim=c(0,3.5))
abline(v=1,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals",cex=0.85)
plot(density(BadBs),main=" ",xlab="Estimated Slope",ylim=c(0,3))
abline(v=1,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals",cex=0.85)
dev.off()

pdf("NonLinBadTs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OKTs,BadTs,pch=20,xlab="T-statistics: Normal residuals",
     ylab="T-statistics: Skewed residuals")
abline(a=0,b=1,lwd=2,col="black")
abline(lm(BadTs~OKTs),lwd=2,col="red",lty=2)
legend("topleft",bty="n",lwd=c(2,2),col=c("black","red"),
       lty=c(1,2),legend=c("45-degree line","OLS fit"))
text(12,2,"N=20")
dev.off()

# Now, repeat the entire thing for N = 2000:

set.seed(7222009)
for (i in 1:sims) {
  u <- rnorm(NN,0,2) # mean zero, s.d = 2
  # Exponentiate:
  eu <- exp(u)
  eu <- eu-mean(eu) # new residuals are mean-zero
  eu <- (eu/sd(eu))*2 # and also sd = 2
  
  # Generate Ys:
  
  X <- runif(NN,-4,4)
  Y1 <- 0 + 1*X + 1*u 
  Y2 <- 0 + 1*X + 1*eu # same Xs in both
  
  fit1 <- lm(Y1~X)
  fit2 <- lm(Y2~X)
  
  OKBs[i] <- fit1$coefficients[2]
  BadBs[i] <- fit2$coefficients[2]
  OKTs[i] <- summary(fit1)$coefficients[2,3]
  BadTs[i] <- summary(fit2)$coefficients[2,3]
} 

# Plots...

pdf("NonLinBadResidsNN.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(u),main=" ",xlab="Residual Values",ylim=c(0,0.25))
abline(v=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals")
plot(density(eu),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals")
dev.off()

pdf("NonLinBsAreJustFineNN.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(OKBs),main=" ",xlab="Estimated Slope")
abline(v=1,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals",cex=0.85)
plot(density(BadBs),main=" ",xlab="Estimated Slope")
abline(v=1,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals",cex=0.85)
dev.off()

pdf("NonLinBadTsNN.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OKTs,BadTs,pch=20,xlab="T-statistics: Normal residuals",
     ylab="T-statistics: Skewed residuals")
abline(a=0,b=1,lwd=2,col="black")
abline(lm(BadTs~OKTs),lwd=2,col="red",lty=2)
legend("topleft",bty="n",lwd=c(2,2),col=c("black","red"),
       lty=c(1,2),legend=c("45-degree line","OLS fit"))
text(55,48.2,"N=2000")
dev.off()


#############################################
# Real-Data Example:

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/fordham.csv")
Data<-read.csv(text=temp, header=TRUE)
rm(temp)

with(Data, summary(milgdp))
with(Data, summary(gdp))

# Ladder Plot (can also use -ladder- in the HH package):

par(mfrow=c(3,3))
with(Data, plot(density(gdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(gdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(gdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(gdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(gdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(gdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/gdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/gdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/gdp^3,na.rm=TRUE),main="1 / Cubic"))

with(Data, plot(density(milgdp^3,na.rm=TRUE),main="Cubic"))
with(Data, plot(density(milgdp^2,na.rm=TRUE),main="Square"))
with(Data, plot(density(milgdp,na.rm=TRUE),main="Identity"))
with(Data, plot(density(sqrt(milgdp),na.rm=TRUE),main="Square Root"))
with(Data, plot(density(log(milgdp),na.rm=TRUE),main="Log"))
with(Data, plot(density(1/sqrt(milgdp),na.rm=TRUE),main="1 / Square Root"))
with(Data, plot(density(1/milgdp,na.rm=TRUE),main="Inverse"))
with(Data, plot(density(1/milgdp^2,na.rm=TRUE),main="1 / Square"))
with(Data, plot(density(1/milgdp^3,na.rm=TRUE),main="1 / Cubic"))

# Other plots:

par(mfrow=c(2,2))
with(Data, plot(gdp,milgdp))
with(Data, plot(log(gdp),milgdp))
with(Data, plot(gdp,log(milgdp)))
with(Data, plot(log(gdp),log(milgdp)))

# Regressions:

linlin <- with(Data, lm(milgdp~gdp))
summary(linlin)
linlog <- with(Data, lm(milgdp~log(gdp)))
summary(linlog)
loglin <- with(Data, lm(log(milgdp+0.01)~gdp))
summary(loglin)
loglog <- with(Data, lm(log(milgdp+0.01)~log(gdp)))
summary(loglog)

# Residual plot:

pdf("MilSpendGDPResidsDensities-R.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
plot(density(linlog$residuals),xlim=c(-5,10),lwd=3,lty=3,
     col="blue",main="",xlab="Residual Values")
lines(density(linlin$residuals),lwd=3,lty=1,col="black")
lines(density(loglin$residuals),lwd=3,lty=2,col="red")
lines(density(loglog$residuals),lwd=3,lty=4,col="darkgreen")
abline(v=0,lty=2,lwd=1)
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4),
       col=c("black","red","blue","darkgreen"),
       legend=c("Untransformed","Logged Y","Logged X",
                "Both Logged"))
abline(v=0,lwd=1,lty=2)
dev.off()

# fin
