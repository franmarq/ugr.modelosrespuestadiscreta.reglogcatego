#por ser datos agrupados hacemos la siguiente transformacion

Contacto trabajo PCRA PCRB
cFam Ofi 6 13
cTrab Ofi 11 13
cAmi Ofi 8 7
cFam Edu 7 8
cTrab Edu 10 5
cAmi Edu 8 8
cFam Hos 4 17
cTrab Hos 9 11
cAmi Hos 9 7

#lectura de la data

covidTem3<-read.table("covidTem31.txt",header=T,sep=" ")

#obtencion del modelo
Ajuste.Covid.All<-glm(cbind(PCRA,PCRB)~Contacto+trabajo,family=binomial,
                       data=covidTem3)
summary(Ajuste.Covid.All)

fitted.values(Ajuste.Covid.All)

#probabilidad
1-0.293
#Ventaja de 
1/exp(0.49207)

#intervalo de confianza
exp(confint.default(Ajuste.Covid.All))
exp(confint.default(Ajuste.Covid.All,level=0.9))




#modelo con cambio de categoria
covidTem31<-covidTem3
covidTem31$Contacto<-relevel(covidTem31$Contacto,ref="cFam")
Ajuste.Covid.All2<-glm(cbind(PCRA,PCRB)~Contacto+trabajo,family=binomial,
                      data=covidTem31)
summary(Ajuste.Covid.All2)

#test razon versimilitudes

#modelo general ajuste.covid.all
#modelo particular con la variable trabajo

Ajuste.Covid0<-glm(cbind(PCRA,PCRB)~trabajo,family=binomial,
                   data=covidTem3)
#test
anova(Ajuste.Covid0,Ajuste.Covid.All)

pchisq(anova(Ajuste.Covid0,Ajuste.Covid.All)$Deviance[2],
       anova(Ajuste.Covid0,Ajuste.Covid.All)$Df[2],lower.tail=F)


#wilks valor de p a partor de resultado bondad ajuste

pchisq(2.8499,df=4)
1-0.416



////////////////////////////////////////////////////////////////////
  
  
  
  #Ejercicio 2
infarto<-read.csv("infarto.csv",header=TRUE)
str(infarto)
#ajustes
infarto$Infarto<-as.numeric(infarto$Infarto)
infarto$Edad<-as.numeric(infarto$Edad)
infarto$Infarto<-as.numeric(infarto$Infarto)
infarto$Peso<-as.numeric(infarto$Peso)
infarto$Colesterol<-as.factor(infarto$Colesterol)
infarto$Actividad<-as.factor(infarto$Actividad)
#valido cambios
str(infarto)



Ajuste.all<-glm(Infarto~Edad+Colesterol+Peso+Actividad
                ,family=binomial,data=infarto)

summary(Ajuste.all)


# se carga la funcion
hosmerlem.test <- function(y, yhat, g=10, group=F)
{
  colnum<-ncol(y)
  if(group==F)
  {
    cutyhat1 = cut(yhat,breaks =unique(quantile(yhat, probs=seq(0,1, 1/g))), include.lowest=TRUE)
    obs = xtabs(cbind(1 - y[,colnum], y[,colnum]) ~ cutyhat1)
    expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat1)
  }
  else
  {
    y2<-c(rep(seq(0,0,length=nrow(y)),y[,colnum-1]),rep(seq(1,1,length=nrow(y)),y[,colnum]))
    yhat2<-c(rep(yhat,y[,colnum-1]),rep(yhat,y[,colnum]))
    cutyhat1 = cut(yhat2,breaks =unique(quantile(yhat2, probs=seq(0,1, 1/g))), include.lowest=TRUE)
    obs = xtabs(cbind(1 - y2, y2) ~ cutyhat1)
    expect = xtabs(cbind(1 - yhat2, yhat2) ~ cutyhat1)
  }
  chisq.C = sum((obs - expect)^2/expect)
  P.C = 1 - pchisq(chisq.C, g - 2)
  res <- data.frame(c(chisq.C,P.C))
  colnames(res)<- c("Hosmer-Lemeshow Test")
  rownames(res)<- c("X-squared","p.value")
  return(res)
}

#test de bondad Hosmer
hosmerlem.test(infarto,fitted.values(Ajuste.all),g=10,group=F)


#tasa de clasificaciones correctas (aplica para datos no agrupados, si es el caso)

#proporcion exitos en la muestra
table(infarto$Infarto)
#0.7818
86/(86+24)

Categoria.Pred<-ifelse(fitted.values(Ajuste.all)>=0.78,1,0)
table(infarto$Infarto,Categoria.Pred)

(18+53)/(18+53+6+33)


#comparacion de modelos

#MODELO GENERAL Ajuste.all

#MODELO PARTICULAR
Ajuste.all.0<-glm(Infarto~Edad+Actividad
                  ,family=binomial,data=infarto)
#COMPARACION
anova(Ajuste.all.0,Ajuste.all)

#csalacular chi cuadrado
pchisq(107.1,103)
1-0.6286


#modelo con interaccion
Ajuste.all.int<-glm(Infarto~Colesterol*Actividad
                    ,family=binomial,data=infarto)
Ajuste.all.sint<-glm(Infarto~Colesterol+Actividad
                    ,family=binomial,data=infarto)

anova(Ajuste.all.int,Ajuste.all.sint)

pchisq(109.35,105)
1-0.633


#inclusion stepwise
Ajuste.0<-glm(Infarto~1
              ,family=binomial,data=infarto)


Ajuste.step<-step(Ajuste.0,scope=list(lower=Infarto~1,
                                      upper=Infarto~Edad+Colesterol+Peso+Actividad),
                  direction="both")

la seleccion parte del modelo que tiene solo el parametro
independiente


La variable para las que el contraste es significativo aparecen arriba de <none>.
se selecciona la asociada al mnimo
p-valor
la variable a seleccionar sera aquella con menor deviance ya que el estadistico de 
contraste es la diferencia entre la deviance del modelo constante y del modelo al 
introducir la variable.names

Al ingresar Peso (unica signiicativa, por tanto menor deviance)

El estadistico queda como como 115.41-112.87 el cual se obtiene un p-valor de 
x<-115.41-112.87
x
1-pchisq(x,1)

el valor obtenido es 0.1109 el cual es mayor que el valor usual de significacion 0.1, 
por este motivo la variable no es incluida y por ser la unica variable a considerar aqui 
finaliza el stepwise