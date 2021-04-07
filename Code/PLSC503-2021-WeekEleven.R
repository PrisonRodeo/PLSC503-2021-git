##########################################
# Code for PLSC 503 - Spring 2021
#
# Binary Response Models: Practicum
#
##########################################
# Packages, etc.:

require(RCurl)
library(car)
# install.packages("plotrix") # <- as needed
library(plotrix)
# install.packages("ROCR") # <- as needed
library(ROCR)
# install.packages("pROC") # <- as needed
library(pROC)
# install.packages("zeligverse") # uncomment as needed
devtools::install_version("Zelig", version="5.1.6.1",
                          repos="http://cran.us.r-project.org")
require(Zelig) # Note: Zelig can be weird to load, sometimes...
# install.packages("logistf") # uncomment as needed
require(logistf)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places
#################################
# NAFTA example...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/NAFTA.csv")
NAFTA<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(NAFTA)

# Probit:

NAFTA.GLM.probit<-glm(vote~democrat+pcthispc+cope93+DemXCOPE,
                   NAFTA,family=binomial(link="probit"))
summary(NAFTA.GLM.probit)

# Logit:

NAFTA.GLM.fit<-glm(vote~democrat+pcthispc+cope93+DemXCOPE,
                   NAFTA,family=binomial)
summary(NAFTA.GLM.fit)

# Compare logit vs. probit estimates:

comparedf <-data.frame(probit = coefficients(NAFTA.GLM.probit),
                       logit = coefficients(NAFTA.GLM.fit))
lpfit<-lm(logit~probit,data=comparedf)

pdf("NAFTAprobitVsLogit.pdf",6,6)
par(mar=c(4,4,2,2))
with(comparedf, 
     plot(probit,logit,pch=20,xlim=c(-0.5,3.5),
          ylim=c(-2,8),xlab="Logit Estimates",
          ylab="Probit Estimates"))
with(comparedf, text(probit,logit,labels=rownames(comparedf),
                     pos=c(1,3,3,1,4)))
abline(lpfit,lwd=2,lty=2,col="red")
text(1,5.5,col="red",labels=paste0("Adj. R-squared = ",
                         round(summary(lpfit)$adj.r.squared,2)))
dev.off()


# Interactions...

NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]
(NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.GLM.fit)[4,4] + 
  (1)^2*vcov(NAFTA.GLM.fit)[5,5] + 
  2*1*vcov(NAFTA.GLM.fit)[4,5]))

# Same thing, using -linear.hypothesis- in -car-:

linearHypothesis(NAFTA.GLM.fit,"cope93+DemXCOPE=0")

# Predicted values:

preds<-NAFTA.GLM.fit$fitted.values
hats<-predict(NAFTA.GLM.fit,se.fit=TRUE)

# Plotting in-sample predictions:

XBUB<-hats$fit + (1.96*hats$se.fit) 
XBLB<-hats$fit - (1.96*hats$se.fit)
plotdata<-cbind(as.data.frame(hats),XBUB,XBLB)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))
par(mfrow=c(1,2))

with(plotdata, 
plotCI(cope93[democrat==1],plotdata$fit[democrat==1],ui=plotdata$XBUB[democrat==1],
         li=plotdata$XBLB[democrat==1],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))
with(plotdata, 
plotCI(cope93[democrat==0],plotdata$fit[democrat==0],ui=plotdata$XBUB[democrat==0],
         li=plotdata$XBLB[democrat==0],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))

# Plotting Out-of-sample Predictions:

sim.data<-data.frame(pcthispc=mean(NAFTA$pcthispc),democrat=rep(0:1,101),
                       cope93=seq(from=0,to=100,length.out=101))
sim.data$DemXCOPE<-sim.data$democrat*sim.data$cope93

OutHats<-predict(NAFTA.GLM.fit,se.fit=TRUE,newdata=sim.data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

par(mfrow=c(1,2))
both<-cbind(sim.data,OutHats)
both<-both[order(both$cope93,both$democrat),]

plot(both$cope93[democrat==1],both$fit[democrat==1],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==1],both$OutHatsUB[democrat==1],lty=2)
lines(both$cope93[democrat==1],both$OutHatsLB[democrat==1],lty=2)
text(locator(1),label="Democrats")

plot(both$cope93[democrat==0],both$fit[democrat==0],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==0],both$OutHatsUB[democrat==0],lty=2)
lines(both$cope93[democrat==0],both$OutHatsLB[democrat==0],lty=2)
text(locator(1),label="Republicans")

# Odds Ratios:

lreg.or <- function(model)
       {
        coeffs <- coef(summary(NAFTA.GLM.fit))
        lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2])
        or <- exp(coeffs[ ,1])
        uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2])
        lreg.or <- cbind(lci, or, uci)        
        lreg.or
        }

lreg.or(NAFTA.GLM.fit)

####################
# Goodness of fit:

table(NAFTA.GLM.fit$fitted.values>0.5,NAFTA$vote==1)
chisq.test(NAFTA.GLM.fit$fitted.values>0.5,NAFTA$vote==1)

# ROC curves, plots, etc. (using -ROCR-):

NAFTA.GLM.logithats<-predict(NAFTA.GLM.fit,
                       type="response")
preds<-prediction(NAFTA.GLM.logithats,NAFTA$vote)
plot(performance(preds,"tpr","fpr"),lwd=2,lty=2,
       col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

# "Bad" model:

NAFTA.bad<-with(NAFTA,
                glm(vote~pcthispc,family=binomial(link="logit")))
NAFTA.bad.hats<-predict(NAFTA.bad,type="response")
bad.preds<-prediction(NAFTA.bad.hats,NAFTA$vote)
plot(performance(bad.preds,"tpr","fpr"),lwd=2,lty=2,
     col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

# Comparing ROCs:

GoodROC<-roc(NAFTA$vote,NAFTA.GLM.logithats,ci=TRUE)
GoodAUC<-auc(GoodROC)
BadROC<-roc(NAFTA$vote,NAFTA.bad.hats)
BadAUC<-auc(BadROC)

GoodAUC

BadAUC

# Comparison plot:

pdf("TwoROCs.pdf",5,5)
par(mar=c(4,4,2,2))
plot(GoodROC)
lines(BadROC,col="red",lty=2)
dev.off()

####################################
# Separation & Rare Events...
####################################
# Separation:

# Table

Yeas<-t(c(rep(0,times=212),rep(1,times=219)))
Dems<-t(c(rep(0,times=178),rep(1,times=253)))
table(Yeas,Dems)

# Simulated Logits:

set.seed(7222009)
X<-runif(100,min=-5,max=5)
X<-X[order(X)]
Z<-runif(100,min=-5,max=5)
Y<-ifelse(plogis(X+Z)>0.5,1,0)
Y2<-ifelse(plogis(X+0.5*Z)>0.5,1,0)
Y3<-ifelse(plogis(X+0.1*Z)>0.5,1,0)
Ysep<-ifelse(plogis(X)>0.5,1,0)
Yfit<-glm(Y~X,family="binomial")
Y2fit<-glm(Y2~X,family="binomial")
Y3fit<-glm(Y3~X,family="binomial")
Ysepfit<-glm(Ysep~X,family="binomial")

# Plots:

pdf("Separation.pdf",8,7)
par(mar=c(4,4,2,2))
par(mfrow=c(2,2))
plot(X,Y,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Yfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Yfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Yfit))[4],digits=2))))
plot(X,Y2,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y2fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y2fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y2fit))[4],digits=2))))
plot(X,Y3,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Y3fit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Y3fit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Y3fit))[4],digits=2))))
plot(X,Ysep,pch=19,xlab="X",ylab="Y")
lines(X,plogis(predict(Ysepfit)),lwd=3)
legend("topleft",inset=0.04,bty="n",cex=1.2,
       legend=c(paste("Beta =", round(Ysepfit$coefficients[2],digits=2)),
                paste("SE =", round(sqrt(vcov(Ysepfit))[4],digits=2))))
dev.off()

# Toy data:

rm(X,Y,Z)
set.seed(7222009)
Z<-rnorm(500)
W<-rnorm(500)
Y<-rbinom(500,size=1,prob=plogis((0.2+0.5*W-0.5*Z)))
X<-rbinom(500,1,(pnorm(Z)))
X<-ifelse(Y==0,0,X)

summary(glm(Y~W+Z+X,family="binomial"))
summary(glm(Y~W+Z+X,family="binomial",maxit=100,epsilon=1e-16))

# data<-as.data.frame(cbind(W,X,Y,Z))
# write.dta(data,"SepSim.dta") # for the Stata illustration

# Exact logistic regression...
# DO NOT ACTUALLY RUN THIS CODE -- your computer will likely
# freeze up. It's here for example purposes only.
#
# First, install the legacy package "elrm":
#
# install.packages("devtools") <-- uncomment if needed
# devtools::install_version("elrm", "1.2.4")
# library(elrm)

# df <- data.frame(one=1,Y=Y,W=W,Z=Z,X=X)
# toy.elrm <- elrm(Y/one~W+Z+X,interest=~X,dataset=df,
#                  r=4,iter=5000,burnIn=1000)
# summary(toy.elrm)

#########################
# Pets-as-family data example:

PetsURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/Pets.csv"
temp<-getURL(PetsURL)
Pets<-read.csv(textConnection(temp))
rm(temp)

Pets.1<-glm(petfamily~female+as.factor(married)+as.factor(partyid)
            +as.factor(education),data=Pets,family=binomial)
summary(Pets.1)

Pets.2<-glm(petfamily~female+as.factor(married)*female+as.factor(partyid)+
              as.factor(education),data=Pets,family=binomial)
summary(Pets.2)

with(Pets, xtabs(~petfamily+as.factor(married)+female))

Pets.Firth<-logistf(petfamily~female+
                      as.factor(married)*female+as.factor(partyid)+
                      as.factor(education),data=Pets)
Pets.Firth

# Profile Firth profile likelihood:

Pets.profile<-profile(Pets.Firth,
                      variable="femaleMale:as.factor(married)Widowed",
                      firth=TRUE)

# Plot it:

pdf("Notes/PetsProfileL.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Pets.profile)
abline(v=Pets.Firth$coefficients[15],lty=2,lwd=2)
abline(v=0,lty=3,lwd=1)
dev.off()

########################
# Rare Events:
#
# Rare Event bias figure:

N<-c(.1,5,seq(10,10000,by=10))
p4 <- (0.4-0.5) / (N*(0.4)*(1-0.4))
p1 <- (0.1-0.5) / (N*(0.1)*(1-0.1))
p01 <- (0.01-0.5) / (N*(0.01)*(1-0.01))
p001 <- (0.001-0.5) / (N*(0.001)*(1-0.001))

pdf("Notes/RareEventBiasR.pdf",7,6)
par(mar=c(4,4,2,2))
plot(N,p4,t="l",lwd=2,lty=1,col="black",xlab="N",
     ylab="Bias in the Intercept",ylim=c(-0.5,0))
lines(N,p1,t="l",lwd=2,lty=2,col="green")
lines(N,p01,t="l",lwd=2,lty=3,col="yellow")
lines(N,p001,t="l",lwd=2,lty=4,col="red")
legend("bottomright",bty="n",
       legend=c(expression(paste(bar(pi),"=0.4")),
                expression(paste(bar(pi),"=0.1")),
                expression(paste(bar(pi),"=0.01")),
                expression(paste(bar(pi),"=0.001"))),
       col=c("black","green","yellow","red"),
       lty=c(1,2,3,4),lwd=2)
dev.off()

# OR/BKT data:

BKTURL<-"https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/OR.csv"
temp<-getURL(BKTURL)
RE<-read.csv(textConnection(temp))
rm(temp)

# Basic logit:

baselogit<-glm(dispute~democracy+growth+allies+contig+capratio+trade,
               data=RE,family=binomial)
summary(baselogit)

# (Fake) Case-control sampling:

set.seed(7222009)
REones<-RE[RE$dispute==1,]
REzeros<-RE[RE$dispute==0,]
RSzeros<-REzeros[sample(1:nrow(REzeros),1000,replace=FALSE),]
REsample<-data.frame(rbind(REones,RSzeros))
table(REsample$dispute)

sample.logit<-glm(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,family=binomial)
summary(sample.logit)

# RE logit, prior correction...
# 
# Warning: As a rule, this code may not currently (April 
# 2021) work, because Zelig has some serious issues with 
# package dependencies right now. YMMV, etc.

relogit.pc<-zelig(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,model="relogit",tau=405/20448,case.control=c("prior"))
summary(relogit.pc)

# RE logit, weighting correction:

relogit.wc<-zelig(dispute~democracy+growth+allies+contig+capratio+trade,
                  data=REsample,model="relogit",tau=405/20448,case.control=c("weighting"))
summary(relogit.wc)

# fin
