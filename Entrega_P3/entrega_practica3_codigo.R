#Ejercicio para entregar -- PRACTICA 3

setwd("C:/Users/54249/Desktop/Labo_Cate/Practica_3/Entrega_P3")
rm(list=ls())

#Ingreso los datos de cada estacion y su informacion

Azul<-read.table("AZUL.txt")
Catamarca<-read.table("CATAMARCA.txt")
Aeroparque<-read.table("AEROPARQUE.txt")
Chilecito<-read.table("CHILECITO.txt")
Iguazu<-read.table("IGUAZU.txt")
Mendoza<-read.table("MENDOZA.txt")

info_estaciones<-read.table("estaciones.txt")

dato_faltante<-9999.9

#falta la informacion de mendoza asi que las busco y agrego al data frame
info_mendoza<-c("MENDOZA",-32.9,-68.8,769)
info_estaciones<-rbind(info_estaciones,info_mendoza)

#armo una lista vacía con las dimensiones de la cant. de estaciones x la cant. de variables
datos<-array(list(),dim=c(6,9))
colnames(datos)<-c("Estacion","Lat","Long","Altura","Cod","Fecha","T","Td","Presion")

#los primeros datos (primeras 4 columnas) están en info_estaciones. los ingreso
for (i in 1:4){
  datos[,i]<-info_estaciones[,i]
}
for (i in 2:4){
  datos[,i]<-as.numeric(datos[,i])
}

#los codigos son el mismo para todos los datos dentro de cada estacion. los selecciono (dentro de cada lista) y asigno a la 5ta columna del array:
codigos<-c(Azul[[1]][1],Catamarca[[1]][1],Aeroparque[[1]][1],Chilecito[[1]][1],Iguazu[[1]][1],Mendoza[[1]][1])
datos[,5]<-codigos

#el resto de los datos los acomodo en una matriz
todos_datos<-matrix(c(Azul,Aeroparque,Catamarca,Chilecito,Iguazu,Mendoza),nrow=6, byrow=T)

##los agrego al array
for (i in 1:6){
  for (j in 6:9){
    datos[i,j]<-todos_datos[i,j-4]
  }
  
  #reemplazo faltantes por NA
  for (j in 7:9){
    for (k in 1:length(datos[[i,j]])){
      if (datos[[i,j]][k]==dato_faltante){
        datos[[i,j]][k]<-NA
      }
    }
  }
  #cambio a Celsius
  for (j in 7:8){
    for (k in 1:length(datos[[i,j]])){
      datos[[i,j]][k]<-(datos[[i,j]][k]-32)*(5/9)
    }
  }
}


#genero funcion que resuma por estacion, depende solo del array
resumen<-function(lista){
  for (i in 1:nrow(lista)){
    estacion<-lista[[i]]
    cant_datos<-length(lista[[i,9]])
    
    media_T<-mean(lista[[i,7]],na.rm=T)
    desv_est_T<-sd(lista[[i,7]],na.rm=T)
    T_max<-max(lista[[i,7]],na.rm=T)
    T_min<-min(lista[[i,7]],na.rm=T)
    
    media_Td<-mean(lista[[i,8]],na.rm=T)
    desv_est_Td<-sd(lista[[i,8]],na.rm=T)
    Td_max<-max(lista[[i,8]],na.rm=T)
    Td_min<-min(lista[[i,8]],na.rm=T)
    
    media_P<-mean(lista[[i,9]],na.rm=T)
    desv_est_P<-sd(lista[[i,9]],na.rm=T)
    P_max<-max(lista[[i,9]],na.rm=T)
    P_min<-min(lista[[i,9]],na.rm=T)
    
    fecha_inicial<-min(lista[[i,6]])
    fecha_final<-max(lista[[i,6]])
    
    cat("Estacion:",estacion,
        "\n Cant. de datos:",cant_datos,
        "\n Temperatura:",
        "\n Media:", media_T,
        "\n Desvio estandar:",desv_est_T,
        "\n T min:",T_min,"T max:",T_max,
        "\n Temperatura de Rocío (Td):",
        "\n Media:", media_Td,
        "\n Desvio estandar:",desv_est_Td,
        "\n Td min:",Td_min,"Td max:",Td_max,
        "\n Presion:",
        "\n Media:", media_P,
        "\n Desvio estandar:",desv_est_P,
        "\n P min:",P_min,"P max:",P_max,
        "\n")
  }
}


#genero funcion que diga si hay estaciones en un rango de latitud y longitud, depende de estos valores y el array
estaciones_cercanas<-c()
e<-1
region<-function(lista,long_max,long_min,lat_max,lat_min){
  for (i in 1:nrow(lista)){
   if (lista[[i,2]]>=lat_min&lista[[i,2]]<=lat_max&lista[[i,3]]>=long_min&lista[[i,3]]<=long_max){
     estaciones_cercanas[e]<-lista[[i,1]]
   }
    e<-e+1
  }
  if (length(estaciones_cercanas==0)){
    estaciones_cercanas<-"No hay estaciones cercanas"
  }
  return(estaciones_cercanas)
}

#guardo el array en un archivo .Rdata
save(datos,file="Datos_Estaciones.Rdata")

