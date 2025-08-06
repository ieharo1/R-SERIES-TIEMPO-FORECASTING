#Esto es un comentario

x<-6
y<-7
8->X

z<-x+y

edad <- c(20,21,20,18,23,22,20,21)
table(edad)
genero<-c(1,1,2,1,2,2,1,2,1,2)
ingreso<-c(750,850,1200,1500,850,650,500,680,900,1500)
acuerdo<-c(1,5,2,3,5,4,1,2,3,5)

datos<-data.frame(genero, ingreso, acuerdo)
save(datos,file = "datos.RData")