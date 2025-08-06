# El dataset se denomina titanic_train
names(titanic_train)

#Se factoriza la columna Survived para categorizar
titanic_train$Survived <-factor(titanic_train$Survived)

#Cambio de idioma de los contenidos del sexo
titanic_train$Sex <- ifelse(titanic_train$Sex =="male","hombre","mujer")


titanic_train$Embarked <- ifelse(titanic_train$Embarked == "",NA,titanic_train$Embarked)
titanic_train$Cabin <- ifelse(titanic_train$Cabin =="", NA,titanic_train$Cabin)


#Número de Supervivientes
table(titanic_train$Survived)

#El operador %>% es un pipe
sobrev <- titanic_train %>% group_by(Survived) %>% count()
sobrev

#Gráfica de 
ggplot(titanic_train, aes(Survived)) + geom_bar() +
  geom_text(data=sobrev, aes(Survived,y=25,label=n), color="white")+
  xlab("Supervivientes")+ylab("Frecuencia")
  
#Cantidad de pasajeros por clase
#Cuántos hombres y mujeres abordo
