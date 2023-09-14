##Clase 8 practica MATRICES 
A<-matrix(c(1,4,9,5,8,1,8,7,3),nrow=3,ncol=3)
A
A[3,2]
velocidad<- matrix(c(10, NA, 15, 1, 5, NA, 20, NA, 50, NA, 12, 16), ncol =4 )
velocidad
which(is.na(velocidad))
which(is.na(velocidad), arr.ind = T) #arr.ind =T da la posicion de los NA en filas y columnas

#Ejercicio
matriz_pp_verano<-matrix(c(28,40,43,130,153.2,152.9,118.9,135.4,127.2),ncol=3)
dimnames(matriz_pp_verano)<-list(c("PP diciembre","PP enero","PP febrero"),c("Mendoza","Jujuy","Buenos Aires"))
ppmedia<-c(colMeans(matriz_pp_verano,na.rm=F,dims=1L))
ppmedia
matriz_pp_verano<-rbind(matriz_pp_verano,ppmedia)
neuquen<-matrix(c(11.3,12.5,11.9,27.8),ncol=1)
colnames(neuquen)<-"Neuquen"
matriz_pp_verano<-cbind(matriz_pp_verano,neuquen)
ppmedia_enero<-mean(matriz_pp_verano[2,])
