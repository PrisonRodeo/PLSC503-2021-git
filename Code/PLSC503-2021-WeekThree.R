###################################################
# PLSC 503 -- Spring 2021
#
# Code for Week Three ("Multivariate Regression")
###################################################
# setwd() here...
#
# setwd(~/Whatever)
#
# Packages:

library(RCurl) # <- install packages as necessary
library(car) 
library(lmtest)
library(plotrix)

################################
# Added variable plot:

library(RCurl)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the country-level data
rm(url)

Data<-na.omit(Data[c("infantmortalityperK","DPTpct","healthexpGDP")])

fit<-lm(infantmortalityperK~DPTpct,data=Data)
aux<-lm(healthexpGDP~DPTpct,data=Data)

# Plot:

pdf("AVPlot.pdf",7,6)
plot(aux$residuals,fit$residuals,pch=19,
     xlab="Health Expenditures | DPT Rates: Residuals", 
     ylab="Infant Mortality | DPT Rates: Residuals")
abline(lm(fit$residuals~aux$residuals),lwd=3)
dev.off()

# Using avPlots from -car-:

fit2<-lm(infantmortalityperK~DPTpct+healthexpGDP,data=Data)
avPlots(fit2,~healthexpGDP)

#####################
# Toy example (N=4):

Y<-c(4,-2,9,-5)
X1<-c(200,120,430,110)
X2<-c(-17,32,-29,25)
data<-cbind(Y,X1,X2)
scatterplotMatrix(data)

cor(data)

fit<-lm(Y~X1+X2)
summary(fit)

########################################

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2021-git/master/Data/africa2001.csv")
Data<-read.csv(text=temp, header=TRUE)
Data<-with(Data, data.frame(adrate,polity,
                            subsaharan=as.numeric(subsaharan),muslperc,literacy))
summary(Data)
cor(Data)

# Scatterplot matrix:

scatterplotMatrix(Data)

# Linerar model...

model<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
summary(model)

options(digits=4)
vcov(model)

# Linear hypothesis (F) tests...

modelsmall<-lm(adrate~muslperc+literacy,data=Data)
waldtest(model,modelsmall)  # from -lmtest- package

# Or:

linearHypothesis(model,"muslperc=0.1") # from -car-

linearHypothesis(model,"literacy=muslperc")

# Confidence ellipse

confidenceEllipse(model=model,which.coef=c(4,5),
                  xlab="Muslim Percentage",ylab="Literacy")
abline(h=0,v=0,lty=2)

# Predicted values:

hats<-fitted(model)

# Or, alternatively:

fitted<-predict(model,se.fit=TRUE, interval=c("confidence"))

# Plotted:

scatterplot(model$fitted~Data$adrate,log="x",smooth=FALSE,boxplots=FALSE,
            reg.line=FALSE,xlab="Observed HIV Rate",ylab="Predicted HIV Rate",
            pch=16,cex=2)

# Or:

plotCI(Data$adrate,model$fitted,uiw=(1.96*(fitted$se.fit)),
       log="x",xlab="Observed HIV Rate",ylab="Predicted HIV Rate")
lines(lowess(Data$adrate,Data$adrate),lwd=2)

