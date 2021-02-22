#Valentina Diaz Torres
#Ejercicio 1
#---------------Librerias y carga de datos---------------#

library(ggplot2)
library( nycflights13)
data('flights')
data('airports')
attach (flights)
vuelos <- as.data.frame(flights)
attach (airports)

#-------------- Graficos---------------------------------#

# Figura A- Grafico de barras de flights (count y month)

ggplot(flights,aes(x = month))+geom_bar()

#Figura B - Grafico de densidad | Flights x= dep_delay | y = arr_delay

ggplot(flights, aes(x=dep_delay, y=arr_delay) ) +
  geom_density_2d() 

#Figura C - Grafico de puntos | airports x = lon, y = lat

ggplot(airports, aes(lon,lat)) + geom_point(aes(size=alt),alpha= 0.05)

