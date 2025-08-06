library(datos)
library(tidyverse)
view(vuelos)
#Filtros - filter()

filter(vuelos,aerolinea=="UA")

resultado <- filter(vuelos, aerolinea=="UA"&distancia>2000)
view(resultado)

vuelos_ <- filter(vuelos, origen=="JFK"|origen=="EWR")
view(vuelos_)

#Ordenamiento
orden <- arrange(vuelos,distancia)

orden<-arrange(vuelos,desc(distancia))

