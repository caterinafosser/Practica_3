##Examen parcial Labo 1C2023

rm(list=ls())
setwd("/home/clinux01/Desktop/Labo_Cate/Practica_3")

##Ejercicio 1----
fecha<-readline("Ingrese el dia de medicion en formato DD-MM-AA:")
dia<-as.numeric(substr(fecha,1,2))
mes<-as.numeric(substr(fecha,4,5))
anio<-as.numeric(substr(fecha,7,8))

cant_estaciones<-as.numeric(readline("Ingrese la cantidad de estaciones a informar:"))
while(is.na(cant_estaciones)){
cant_estaciones<-as.numeric(readline("Ingrese la cantidad de estaciones a informar:"))
}

nombres<-vector(length = cant_estaciones)
presiones<-vector(length = cant_estaciones)

for (i in 1:cant_estaciones){
  nombres[i]<-readline(paste("Ingrese el nombre de la estacion ",i,":",sep=""))
  repeat{
    presiones[i]<-as.numeric(readline(paste("Ingrese la presion medida de la estacion ",i," (hPa):",sep="")))
      if (!is.na(presiones[i])){
        break
      }
    }
  if (presiones[i]>1013.5){
    excedente<-presiones[i]-1013.5
    cat("La estacion meteorologica",nombres[i],"el dia",dia,"de",mes,"del",anio,"se encuentra bajo condiciones de ALTA PRESION y estuvo por encima del valor de 1013.5 hPa en",excedente,"hPa")
  }else if (presiones[i]==1013.5){
    cat("La estacion meteorologica",nombres[i],"el dia",dia,"de",mes,"del",anio,"se encuentra bajo condiciones NORMALES DE PRESION")
  }else if (presiones[i]<1013.5){
    deficit<-1013.5-presiones[i]
    cat("La estacion meteorologica",nombres[i],"el dia",dia,"de",mes,"del",anio,"se encuentra bajo condiciones de BAJA PRESION y estuvo por debajo del valor de 1013.5 hPa en",deficit,"hPa")
  }
}

##Ejercicio 2----
set.seed(123)
datos<-rnorm(19716,mean=25,sd=5)
lat<-seq(23,38,by=0.5)
lon<-seq(64,51,by=-0.25)
tiempos<-(length(datos)/(length(lat)*length(lon)))

temp<-array(datos,dim=c(length(lon),length(lat),tiempos),dimnames = list(c(lon),c(lat),c(seq(1,tiempos))))

#marzo ---> mes 3

temp_marzo<-temp[,,3]

t_min<-temp_marzo[which(temp_marzo==min(temp_marzo))]
pos_t_min<-which(temp_marzo==min(temp_marzo),arr.ind=T)
lon_t_min<-as.numeric(rownames(temp_marzo)[pos_t_min[1]])
lat_t_min<-as.numeric(rownames(temp_marzo)[pos_t_min[2]])

t_max<-temp_marzo[which(temp_marzo==max(temp_marzo))]
pos_t_max<-which(temp_marzo==max(temp_marzo),arr.ind=T)
lon_t_max<-as.numeric(rownames(temp_marzo)[pos_t_max[1]])
lat_t_max<-as.numeric(rownames(temp_marzo)[pos_t_max[2]])

cat("El valor maximo de temperatura fue de",t_max,"en la longitud",lon_t_max,"°O y la latitud",lat_t_max,"°S")
cat("El valor minimo de temperatura fue de",t_min,"en la longitud",lon_t_min,"°O y la latitud",lat_t_min,"°S")

tabla_temp_marzo<-data.frame("Longitud"=c(lon_t_min,lon_t_max),"Latitud"=c(lat_t_min,lat_t_max),"Temperatura"=c(round(t_min,digits=1),round(t_max,digits=1)),row.names = c("Temp max","Temp min"))


#SON --> meses 9,10,11
temp_SON<-temp[,,c(9,10,11)]
temp_media_SON<-apply(temp_SON,c(1,2),mean)

desvio<-sd(temp_media_SON)
mediana<-median(temp_media_SON)
estadisticos_SON<-list("Desvio_estandar"=desvio,"Mediana"=mediana)
valor_medio<-mean(temp_media_SON)
estadisticos_SON$Valor_medio<-valor_medio


v=seq(1,120,by=2)

matrix(c(2,3,2,3,2,3),nrow=2,dimnames=list(c("hola","chau"),c("1","2","3")))
colnames()
       