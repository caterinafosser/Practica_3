##Practica integradora 2

setwd("C:/USers/54249/Desktop/Labo_Cate/Practica_3")

##2
datos_PP[datos_PP==-99]<-NA

datos_stats<-function(datos){
  media<-mean(datos_PP,na.rm=T)
  desvio<-sd(datos_PP,na.rm=T)
  maximo<-max(datos_PP,na.rm=T)
  return(list(media,desvio,maximo))
}
datos_stats(datos_PP)

cant_regiones<-(dim(datos_PP)[1]*dim(datos_PP)[2])/25

datos_regiones<-array(datos_PP,dim=c(5,5,625,8784))

apply(datos_regiones,MARGIN = c(3,4),FUN = datos_stats(datos_regiones))
for (i in )
datos_stats(datos_regiones[])