################################################
# PLSC 503 -- Spring 2021: Code for Week Five.
################################################

# Install packages as needed...

library(RCurl)
library(plyr)
library(car)
library(rms)
library(plm)
library(lmtest)
# install.packages("stargazer")
library(stargazer)
# install.packages("MASS")
library(MASS)

# setwd() too...

# Options...

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
################################################
# Handy "robust" summary function for lm:

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)
################################################
# Data:
#
# ANES 2016 pilot study aggregation example...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/ANES-pilot-2016.csv")
ANES<-read.csv(text=temp, header=TRUE)
rm(temp)

ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)

# States:

ANES$State<-recode(ANES$state,
"1='AL';2='AK';4='AZ';5='AR';6='CA';8='CO';9='CT';
10='DE';11='DC';12='FL';13='GA';15='HI';16='ID';17='IL';
18='IN';19='IA';20='KS';21='KY';22='LA';23='ME';24='MD';
25='MA';26='MI';27='MN';28='MS';29='MO';30='MT';31='NE';
32='NV';33='NH';34='NJ';35='NM';36='NY';37='NC';38='ND';
39='OH';40='OK';41='OR';42='PA';44='RI';45='SC';46='SD';
47='TN';48='TX';49='UT';50='VT';51='VA';53='WA';54='WV';
55='WI';56='WY'")

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(State),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE))
summary(StateFT)

respfit<-with(StateFT, lm(meantherm~log(Nresp)))

pdf("StateThermPlot.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",xlim=c(0.5,200),
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
dev.off()

#########################################
# What do "robust" SEs do? A simulation:

set.seed(7222009)
X <- rnorm(10)
Y <- 1 + X + rnorm(10)
df10 <- data.frame(ID=seq(1:10),X=X,Y=Y)

fit10 <- lm(Y~X,data=df10)
summary(fit10)
rob10 <- vcovHC(fit10,type="HC1")
sqrt(diag(rob10))

# "Clone" each observation 100 times

df1K <- df10[rep(seq_len(nrow(df10)), each=100),]
df1K <- pdata.frame(df1K, index="ID")

fit1K <- lm(Y~X,data=df1K)
summary(fit1K)
summary(fit1K, cluster="ID")

###########################
# Justices data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/Justices.csv")
Justices<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Justices)

OLSfit<-with(Justices, lm(civrts~score))
summary(OLSfit)

WLSfit<-with(Justices, lm(civrts~score,weights=lnNedit))
summary(WLSfit)

pdf("WLSBubblePlotR.pdf",6,6)
par(mar=c(4,4,2,2))
with(Justices, symbols(score, civrts,circles=Neditorials,
        ylab="Civil Rights Voting",xlab="Segal-Cover Score",
        ylim=c(0,100)))
abline(reg=OLSfit,lwd=2)
abline(reg=WLSfit,lwd=2,lty=2)
with(Justices, points(score,civrts,pch=20))
legend("topleft",bty="n",lty=c(1,2),lwd=2,
       legend=c("OLS","WLS"))
dev.off()

# "Robust"

hccm(OLSfit, type="hc1")

OLSfit2<-ols(civrts~score, x=TRUE, y=TRUE)
RobSEs<-robcov(OLSfit2)
RobSEs

#########################################
# Collinearity, etc.
#########################################
#
# Africa data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

# Perfect multicollinearity:

Africa$newgdp<-(Africa$gdppppd-mean(Africa$gdppppd))*1000

fit<-with(Africa, lm(adrate~gdppppd+newgdp+healthexp+subsaharan+
                       muslperc+literacy))
summary(fit)

# N = K

smallAfrica<-subset(Africa,subsaharan=="Not Sub-Saharan")
fit2<-with(smallAfrica,lm(adrate~gdppppd+healthexp+muslperc+
                            literacy+war))
summary(fit2)

# Multicollinearity examples:

with(Africa, table(internalwar,intensity))

HIV1<-with(Africa, lm(adrate~internalwar))
HIV2<-with(Africa, lm(adrate~intensity))
HIV3<-with(Africa, lm(adrate~internalwar+intensity))

stargazer(HIV1,HIV2,HIV3)

##################
# NAFTA example...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/impeachment.csv")
impeachment<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(impeachment)

fit<-with(impeachment,
          lm(votesum~ADA98+GOPmember+clint96+pctbl96+unionpct))
summary(fit)

idata=impeachment[c(-1,-2)]
cor(idata)

vif(fit)

# Drop GOP...

fit2<-with(impeachment,
           lm(votesum~ADA98+clint96+pctbl96+unionpct))
summary(fit2)

vif(fit2)

# Ridge regression:

ridge.vote<-lm.ridge(votesum~ADA98+GOPmember+clint96+pctbl96+unionpct,
                     data=impeachment,lambda=seq(1,5001,10))
select(ridge.vote)

# Plot:

pdf("RidgePlotInR.pdf",6,5)
par(mar=c(4,4,2,2))
matplot(ridge.vote$lambda,t(ridge.vote$coef),type="l",
        xlab=paste("log(lambda)"),
        ylab=expression(hat(beta)),lwd=2,log="x")
legend("topright",legend=c("ADA","GOP","Clinton '96","Pct Black",
                          "Union Pct."), lty=c(1,2,3,4,5),
                 col=c(1,2,3,4,5),cex=0.8,bty="n")
abline(h=0)
abline(v=10)
dev.off()