
######################################### 
title: 'CP001: Regresión y Salarios NBA'#
author: "Valentina Díaz Torres"         #
date: "October 28, 2020"                #
######################################### 

library(dplyr)
library(MASS)
require(lmtest)
library(readxl)
nba=read.csv("nba.csv")
attach(nba)
data<- na.omit(nba)
set.seed(1234)
################### Modelo 1###############################

regres01 = lm(Salary~NBA_DraftNumber+Age+G+MP+PER+TS.+X3PAr+FTr+ORB.+DRB.+TRB.+AST.+STL.+BLK.+TOV.+USG.+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP,data)
summary(regres01)
library(WRS2)
anova(regres01)
stepAIC(regres01, direction = "both") 

################### Modelo 2###############################
regres02<- lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + X3PAr + ORB. + TRB. + USG. + WS + OBPM, data = data)
summary(regres02)
#valoración global
library(gvlma)
gvlma(regres02)
anova(regres02)
vif(regres02) 
vif(regres02) > 2
BIC(regres01,regres02) 

################### Modelo 3###############################

regres03<- lm(Salary ~ NBA_DraftNumber + Age+ TRB.+ WS, data = data)
summary(regres03)
vif(regres03) 
#-------------------------------------------------------
################### MODELO FINAL##########################
#-------------------------------------------------------
regres04<- lm(Salary ~ NBA_DraftNumber + Age+ WS, data = data)
summary(regres04) 
gvlma(regres04)
vif(regres04) 
#test Durbin-Watson
dwtest(regres04)
#qqplot estudio de la normalidad
library(car)
qqPlot(regres04, labels=row.names(nba), id.method="identify",  
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres04, labels=row.names(nba), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#test Jarque Bera
library(fBasics)
library(akima)
vResid4=resid(regres04)
jbTest(vResid4) 
#SHAPIRO-WILK
shapiro.test(vResid4)  
#LINEALIDAD
crPlots(regres04)
#Estudio de los residuos
residuos04<- residuals(regres04)
hist(residuos04)
require(lmtest)
residplot4 <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot4(regres04)   
#INTERACCIÓN
regresInter=lm(Salary~NBA_DraftNumber*Age*WS,data=nba) 
#OUTLIERS
outlierTest(regres04)
#Eliminación de outliers
boxplot.stats(nba$Salary)
sal_out <- nba$Salary[nba$Salary < 23000000]
boxplot.stats(sal_out)
sal_out2 <- sal_out[sal_out < 17745894]
boxplot.stats(sal_out2) 
sal_out3 <- sal_out2[sal_out2 < 14275000]
boxplot.stats(sal_out3)
sal_out4 <- sal_out3[sal_out3 < 12584270]
boxplot.stats(sal_out4) 
sal_out5 <- sal_out4[sal_out4 < 11422536]
boxplot.stats(sal_out5)  
sal_out6 <- sal_out5[sal_out5 < 9821429]
boxplot.stats(sal_out6)   
sal_out7 <- sal_out6[sal_out6 < 8406000]
boxplot(sal_out7, horizontal = TRUE)

#NBA_DraftNumber
boxplot(nba$NBA_DraftNumber , horizontal = TRUE)
boxplot.stats(nba$NBA_DraftNumber)

#WS
boxplot.stats(nba$WS)
ws_out <- nba$WS[nba$WS < 8.7]
boxplot.stats(ws_out)
ws_out1 <- nba$WS[nba$WS < 7.9]
boxplot(ws_out1, horizontal = TRUE)
boxplot.stats(ws_out1)
#VALORES EXTREMOS
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regres04)



