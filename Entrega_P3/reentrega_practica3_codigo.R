#Ejercicio para entregar -- PRACTICA 3 -- REENTREGA

setwd("C:/Users/54249/Desktop/Labo_Cate/Practica_3/Entrega_P3")
rm(list=ls())


##1
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
info_mendoza<-data.frame("MENDOZA",-32.9,-68.8,769)
colnames(info_mendoza)=c("V1","V2","V3","V4")
info_estaciones<-rbind(info_estaciones,info_mendoza)

#hago una lista para cada estacion con su informacion, contenida en 2 data frames
datos_azul<-list(Azul,info_estaciones[1,])
datos_catamarca<-list(Catamarca,info_estaciones[3,])
datos_aeroparque<-list(Aeroparque,info_estaciones[2,])
datos_chilecito<-list(Chilecito,info_estaciones[4,])
datos_iguazu<-list(Iguazu,info_estaciones[5,])
datos_mendoza<-list(Mendoza,info_estaciones[6,])


#armo la lista de listas
datos_todos<-list("Azul"=datos_azul,"Catamarca"=datos_catamarca,"Aeroparque"=datos_aeroparque,"Chilecito"=datos_chilecito,"Iguazu"=datos_iguazu,"Mendoza"=datos_mendoza)

#reemplazo datos faltantes con NA
for (i in 1:length(datos_todos)){
  datos_todos[[i]][[1]][datos_todos[[i]][[1]]==dato_faltante]<-NA
}

#cambio de fahrenheit a celsius en las columnas que corresponden a T y Td dentro del 1er data frame de cada estacion (1er elem de cada lista)
for (i in 1:length(datos_todos)){
  datos_todos[[i]][[1]][3:4]<-(datos_todos[[i]][[1]][3:4]-32)*(5/9)
}


##2
#i) genero funcion que resuma por estacion, depende solo de la lista de listas
resumen<-function(lista){
  for (i in 1:length(lista)){
    estacion<-as.character(lista[[i]][[2]][["V1"]])
    cant_datos<-nrow(datos_todos[[i]][[1]])
    
    media_T<-mean(lista[[i]][[1]][["V3"]],na.rm=T)
    desv_est_T<-sd(lista[[i]][[1]][["V3"]],na.rm=T)
    T_max<-max(lista[[i]][[1]][["V3"]],na.rm=T)
    T_min<-min(lista[[i]][[1]][["V3"]],na.rm=T)
    
    media_Td<-mean(lista[[i]][[1]][["V4"]],na.rm=T)
    desv_est_Td<-sd(lista[[i]][[1]][["V4"]],na.rm=T)
    Td_max<-max(lista[[i]][[1]][["V4"]],na.rm=T)
    Td_min<-min(lista[[i]][[1]][["V4"]],na.rm=T)
    
    media_P<-mean(lista[[i]][[1]][["V5"]],na.rm=T)
    desv_est_P<-sd(lista[[i]][[1]][["V5"]],na.rm=T)
    P_max<-max(lista[[i]][[1]][["V5"]],na.rm=T)
    P_min<-min(lista[[i]][[1]][["V5"]],na.rm=T)
    
    fecha_inicial<-min(lista[[i]][[1]][["V2"]])
    fecha_final<-max(lista[[i]][[1]][["V2"]])
    
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


#ii) genero funcion que diga si hay estaciones en un rango de latitud y longitud, depende de estos valores y el array

region<-function(lista,lat_min,lat_max,long_min,long_max){
  estaciones_cercanas<-c()  #genero vector donde se van a guardar los nombres de las estaciones
  for (i in 1:length(lista)){  #recorro cada estacion dentro del array
   if (lista[[i]][[2]][["V2"]]>=lat_min&lista[[i]][[2]][["V2"]]<=lat_max&lista[[i]][[2]][["V3"]]>=long_min&lista[[i]][[2]][["V3"]]<=long_max){ #condiciones de lat y long
     estaciones_cercanas<-c(estaciones_cercanas,lista[[i]][[2]][["V1"]]) #si cond se cumple, agrego el nombre a mi vector
   }
  }
  if (length(estaciones_cercanas)==0){
    estaciones_cercanas<-"No hay estaciones cercanas"
  }
  return(cat("Las estaciones que se encuentran dentro de la region son:",estaciones_cercanas))
}

#iii) guardo el array en un archivo .Rdata
save(datos_todos,file="Datos_Estaciones.Rdata")
