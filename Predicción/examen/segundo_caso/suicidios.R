###################################
#Valentina Díaz Torres
#Examen de Predicción 2020-2021. MDS, CUNEF
#PREGUNTA 2
###################################
#Libraries

library(readr)
library(skimr)
library(ggplot2)
library(gplots)
library(dplyr )
library(graphics)
library(corrplot)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(car)
library(mctest)
library(tidyverse)
library(foreign)
library(varhandle)
require(fpc)
library(gridExtra)
library(ggExtra)
library(forecast)
library(foreign)
library(astsa)
library(tseries)
library(tsoutliers)

#Load data
suicidios <- read_delim("Suicidios.csv",";", escape_double = FALSE, col_types= cols(Date=col_date(format="%b-%y")),trim_ws = TRUE)
View(suicidios)

#Renombrar columnas con espacios
names(suicidios)[3]<-"Suicide_Google"
names(suicidios)[4]<-"Depression_Google"

#exploracion de la base de datos
colnames(suicidios)
skim(suicidios)
sum(is.na(suicidios))#no hay valores nas
colnames(suicidios)
head(suicidios)
tail(suicidios)
unique(suicidios$Date) #todas las fechas

#graficos

suicidios_depresion <- ggplot(suicidios, aes(x=Suicide_Google, y=Depression_Google, color="salmon")) +
  geom_point() +
  theme(legend.position="none")
suicidios_depresion_2 <- ggMarginal(suicidios_depresion, type="histogram", fill = "slateblue", xparams = list(  bins=10))
suicidios_depresion_2 


#---------------------SERIES TERMPORALES--------#

suicidios_d <- ts(suicidios$Suicides, start = 2004, frequency = 12)
G_suicidios <- ts(suicidios$Suicide_Google, start = 2004, frequency = 12)
G_depresion <- ts(suicidios$Depression_Google, start = 2004, frequency = 12)
datos <- ts(suicidios[,c(2:4)], start = 2004, frequency = 12)

print(suicidios_d)
print(G_suicidios)
print(G_depresion)

#plot de las series temporales de las tres columnas representadas
autoplot(datos, facets = TRUE)


google <- cbind(G_suicidios,G_depresion)
google

#plot Suicidios Vs búsquedas en Google

datos_2 <- cbind(google, suicidios_d) 

  autoplot(datos_2, facets = FALSE) +
  ggtitle("Suicidios Vs búsquedas en Google") +
  xlab("meses") +
  ylab("nº de suicidios")

#palabra suicidio a lo largo de los años
g_s<-ggseasonplot(G_suicidios, month.labels = TRUE, month.labels.left = TRUE) +
    ylab("nº de búsquedas") +
    ggtitle("Búsquedas en Google")

#palabra depresion a lo largo de los años
g_d<-ggseasonplot(G_depresion, month.labels = TRUE, month.labels.left = TRUE) +
  ylab("nº de búsquedas") +
  ggtitle("Búsquedas en Google")

s<-ggseasonplot(suicidios_d, month.labels = TRUE, month.labels.left = TRUE) +
  ylab("nº de suicidios") +
  ggtitle("nº de suicidios")
s

grid.arrange(g_s,g_d,s)

qp.s<-qplot(G_depresion, suicidios_d) +
  ylab("suicidios") + xlab("palabra depresión")

qp.d<-qplot(G_suicidios, suicidios_d) +
  ylab("suicidios") + xlab("palabras suicidio")

grid.arrange(qp.s,qp.d)

#Parece que hay una relativa relación de a más palabras buscadas, mayor número de suicidios.

#diferencias que hay que hacer para que sea estacionaria la media


##----ESTACIONALIDAD--##

s_l<-log(suicidios_d)
plot(s_l) #no se ha conseguido estacionalidad

d_l<- log(G_depresion)
plot(d_l)#no se ha conseguido estacionalidad

sg_l<-log(G_suicidios)
plot(sg_l)#no se ha conseguido estacionalidad

#Son estacionarias?#---TEST DE DICKEY FULLER--#

adf.test(s_l,alternative="stationary") #suicidios estacionario
adf.test(d_l, alternative="stationary") #no estacionaria (>0.05)
adf.test(sg_l, alternative="stationary") #no estacionaria (>0.05)

#calculo de diferencias

serie_dif_sg<-diff(G_suicidios)
serie_dif_d<-diff(G_depresion)

#la media se hace estacionaria
plot(serie_dif_sg)
plot(serie_dif_d)

#de nuevo se hace la prueba de dickey fuller
adf.test(serie_dif_sg) #estacionaria
adf.test(serie_dif_d)  #estacionaria

plot(serie_dif_sg, type="o", lty="dashed",col="blue", main="Serie de palabra suicidio")
plot(serie_dif_d, type="o", lty="dashed",col="red", main="Serie de palabra depresión")

#calcular el numero de medias moviles que se va a necesitar

#graficos de autocorrelacion (acf) y autocorrelacion parcial(pacf)

acf(serie_dif_sg,frecuency=1) #ruido blanco #numero de medias moviles
pacf(serie_dif_sg, frecuency=1) #numero de autoregresivos
acf(serie_dif_d, frecuency=1)#ruido blanco
pacf(serie_dif_d,  frecuency=1)
acf(s_l,  frecuency=1)#ruido blanco
pacf(s_l,  frecuency=1)


## ---MODELO ARIMA--#

fit_suicidios <- auto.arima(suicidios_d, seasonal = TRUE) #(0,1,1)(1,0,0)
fit_suicidios
xreg_v <- cbind(G_depresion,G_suicidios)
fit_google <-auto.arima(suicidios_d,xreg=xreg_v, seasonal = TRUE)#(1,1,1)(1,0,0)
fit_google
tsdiag(fit_suicidios)
tsdiag(fit_google)
summary(fit_google) #MAPE=5.985509
summary(fit_suicidios)#MAPE=6.222639

#comprobar los residuos-- TEST LJUNG-BOX--
checkresiduals(fit_google)
checkresiduals(fit_suicidios)
#en ambos casos, p-value es > 0.05, por lo tanto son ruido blanco.

# Estudio de los outliers


(google_d_outlier <- tso(G_depresion, types = c("TC", "AO", "LS", "IO", "SLS")))#en 33(TC) y 85(IO) (09-2006 y 01-2011)
(google_s_outlier <- tso(G_suicidios, types = c("TC", "AO", "LS", "IO", "SLS")))#en 2(IO),19(AO),32(LS),95(IO), 108(AO) --> 02-2004,07-2005,08-2006,11-2011,12-2012
(suicidios_outlier <- tso(suicidios_d, types = c("TC", "AO", "LS", "IO", "SLS")))#ningun outlier
#se deberian eliminar los outliers de google, para poder predecirlo

#representacion de los outliers
plot(google_d_outlier)
summary(google_d_outlier)

plot(google_s_outlier)
summary(google_s_outlier)


(outliers_gs_idx <- google_d_outlier$outliers$ind) #33 85
(outliers_gd_idx <- google_s_outlier$outliers$ind) #2  19  32  95 108

google_outlier<- cbind(google_d_outlier,google_s_outlier)

modelo_google_outlier <- auto.arima(suicidios_d, xreg=google_outlier$yadj, seasonal = TRUE)

checkresiduals(modelo_google_outlier)

#representacion final de las predicciones con gráfico de forecast

modelo_google_outlier %>% forecast(h = 16) %>% autoplot()
modelo_suicidios %>% forecast(h = 16) %>% autoplot()

#evaluaciones finales de ambos modelos
summary(fit_suicidios)
summary(modelo_google_outlier) #el MAPE ahora ha subido a 6.22
