Chapman.Cuali<-read.csv("Chapman_Cuali.csv",header=T,sep=",")
is.numeric(Chapman.Cuali$Id)
## [1] TRUE
is.numeric(Chapman.Cuali$Edad)
## [1] TRUE
is.numeric(Chapman.Cuali$Colesterol)
## [1] TRUE
is.factor(Chapman.Cuali$Presion)
## [1] TRUE
is.factor(Chapman.Cuali$IMC)
## [1] TRUE
is.numeric(Chapman.Cuali$Coronarios)


#El programa R trata a las variables de tipo factor como si fueran de tipo
#entero, en el sentido de que les asigna un orden. Por defecto el orden que se
#asigna es el alfabetico como se puede ver con la sentencia

levels(Chapman.Cuali$IMC)
