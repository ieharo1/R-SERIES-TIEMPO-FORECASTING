datos <-titanic_train
glimpse(datos)
head(datos)
datos$Survived <-if_else(datos$Survived==1, "Si", "No")
datos$Survived <- as.factor(datos$Survived)
datos$Pclass <- as.factor(datos$Pclass)
datos$SibSp <- as.factor(datos$SibSp)
datos$Sex <- as.factor(datos$Sex)
datos$Age <- as.factor(datos$Age)
datos$Embarked <- as.factor(datos$Embarked)
#Número de filas
nrow(datos)
#Averigua si existen observaciones incompletas
any(!complete.cases(datos))
#Averigua si existen valores nulos
map_dbl(datos, .f=function(x){sum(is.na(x))})
#Conocer qué variables tienen contenido ""
datos %>% map_lgl(.f = function(x){any(!is.na(x))})
datos$Cabin[datos$Cabin==""] <- NA
datos$Embarked[datos$Embarked == ""] <- NA
#Niveles para categorizar columna
table(datos$Survived)
levels(datos$Embarked)
prop.table(table(datos$Survived)) %>% round(digits = 2)
n_observaciones <- nrow(datos)
n_observaciones
predicciones <-rep(x="No", n_observaciones)
mean(predicciones==datos$Survived)*100
ggplot(datos, aes(x=Survived, y=..count..,fill=Survived))+
  geom_bar()+
  scale_fill_manual(values=c("gray50","Orange"))

#-----------------------------------------------------------------------------------------------------------------
#CONSULTA GGPLOT2 HISTOGRAMA
#Isaac Haro
# Basic histogram
ggplot(datos, aes(x=Sex)) + geom_histogram(aes(y = ..count..,fill=Survived), stat = "count") + 
  scale_fill_manual(values=c("gray70","Blue"))+labs(title = 'Titulo',
                                                    fill = 'vs motor',
                                                    x = 'Sexo',
                                                    y = 'Supervivencia',
                                                    subtitle = 'agregar información extra',
                                                    caption = 'esta información es de tal fuente')

Bibliografia:
  https://gonzalezgouveia.com/como-hacer-histogramas-en-r-con-base-graphics-y-ggplot2/
  
  