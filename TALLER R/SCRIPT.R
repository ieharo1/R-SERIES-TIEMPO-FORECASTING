install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

DATA <- read_spss("vivienda2022.sav")

DATA
names(DATA)
DATA <- select(DATA, Area=area, Ciudad=ciudad, Tipovivienda=vi02, Numdormitorios=vi07, Vehículo=vi151)
View(DATA)

DATA <- mutate(DATA,vehiculoF, recode(DATA$Vehículo,"1"="Si","2"="No","99"="Tal vez"))




CIFRAS <- read_spss("cifras.sav")
CIFRAS <- select(CIFRAS, sexo=GÉNERO, estatura=EST, peso=PESO, hermanos=HERM, internet=INTERNET, gastos=GASTO)
View(CIFRAS)
table(CIFRAS$sexo)

#HOMBRE - MUJER
CIFRAS$sexo<-as.numeric(CIFRAS$sexo)
CIFRAS<-mutate(CIFRAS,sexom=recode(CIFRAS$sexo,`6`=1,`9`=0))
table(CIFRAS$sexom)
str(CIFRAS$sexom)
CIFRAS$sexom <- as.factor(CIFRAS$sexom)
table(CIFRAS$sexom)
CIFRAS$sexom = factor(CIFRAS$sexom, levels = levels(CIFRAS$sexom),labels=c("Hombre","Mujer"))
table(CIFRAS$sexom)

#INTERNET
CIFRAS$internet<-as.numeric(CIFRAS$internet)
CIFRAS<-mutate(CIFRAS,inter=recode(CIFRAS$internet,`2`=1,`1`=0))
#CIFRAS<-mutate(CIFRAS,inter=recode(CIFRAS$internet,`2`="SÍ",`1`="NO"))
table(CIFRAS$inter)
str(CIFRAS$inter)
CIFRAS$inter <- as.factor(CIFRAS$inter)
table(CIFRAS$inter)
CIFRAS$inter = factor(CIFRAS$inter, levels = levels(CIFRAS$inter),labels=c("NO","SÍ"))
table(CIFRAS$inter)
summary(CIFRAS)