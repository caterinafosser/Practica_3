#Ejercicio Array
viento_prom<-apply(viento,c(1,2,4),mean) #promedio TEMPORALMENTE (dimension 3) sobre long,lat y para los 2 niveles
promedio_850<-viento_prom[,,1]  #me quedo solo con los promedios del 1er nivel
prom_viento_dominio<-mean(promedio_850)

promedio_200<-viento_prom[,,2]
suma_prom_200<-c(sum(promedio_200))

#Ejercicio Data Frame
base<-c(2,4,8,6,5)
altura<-c(1,6,3,2,7)
parcelas<-data.frame(base,altura)
area<-base*altura
parcelas$area<-area  #creo la columna q estoy agregando
