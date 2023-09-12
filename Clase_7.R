###Clase Practica VECTORES ### Practica
#Vectores----
vector_logico=c(TRUE,FALSE,TRUE)
vector_numerico<-c(1,3,5,7)
nuevo_vector_1<-c(vector_logico,vector_numerico)
vector_texto <-c("arbol", "casa", "pez")
nuevo_vector_1 <- c(vector_texto, vector_numerico)

assign("x",c(1,2,5,7))
head(vector_numerico,n=2)  ##primeros elementos
tail(vector_numerico,n=2) ##ultimos elementos

a<-seq(0,10,by=2)
length(a)
cuarto<-a[4]
tres_primeros<-a[1:3]

which(a>5)
mayores_5<-a[which(a>5)]
#equivalente a
mayores_5<-a[a>5]
b<- c( 25, 8, 6, 50 , 10, 0)
b[b<15]


#Ejercicio ----
rm(list=ls())
set.seed(11111)
Datos<-15:97
HR<-sample(Datos,31,replace=TRUE)
ocurrencia_pp<-sample(c(1,0),31,replace=T)
#1
HR_media<-mean(HR)
#2
dias_saturados<-which(HR>90)
valores_saturados<-HR[HR>90]
#3
ocurrencia_pp[dias_saturados]
#4
ocurrencia_pp[which(ocurrencia_pp==1)]->dias_con_pp
sum(dias_con_pp)

#Ejercicio compliqueti----
TF<-c()
R<-c()
HI<-c()
for (i in 1:5){
  TF<-c(TF,as.numeric(readline(paste("Ingrese, en °F, el valor de TF del dia ",i,":",sep=""))))
  R<-c(R,as.numeric(readline(paste("Ingrese, en % el valor de HR del dia ",i,":",sep=""))))
  HI[i]<--42.379 + 2.04901523* TF[i] + 10.14333127* R[i] - 0.22475541 * TF[i] *R[i] - 6.83783 * 10**-3 *TF[i]**2-5.481717*10**-2*R[i]**2+ 1.22874*10**-3*TF[i]**2*R[i] + 8.5282 * 10**-4*TF[i]*R[i]**2- 1.99 *10**-6*TF[i]**2*R[i]**2
  if (R[i]<13&&TF[i]>80&&TF[i]<112){
    HI[i]=HI[i]--((13-R[i])/4)*sqrt((17-abs(TF[i]-95.))/17)
  }else if (R[i]>85&&TF[i]>80&&TF[i]<87){
    HI[i]=HI[i]+-((R[i]-85)/10) * ((87-TF[i])/5)
  }else if (HI[i]<80){
    HI[i] = 0.5 * (TF[i] + 61 + ((TF[i]-68)*1.2) + (R[i]*0.094))
  }
}
msj<-c()
for (i in 1:length(HI)){
  if (HI[i]>=80&&HI[i]<90){
    msj[i]<-"Precaucion - Posible fatiga con exposicion prolongada y/o actividad fisica."
  }else if (HI[i]>=90&&HI[i]<103){
    msj[i]<-"Extrema Precaucion - Es posible un golpe de calor, calambres por calor o agotamiento por calor con exposicion prolongada y/o actividad fisica."
  }else if (HI[i]>=103&&HI[i]<124){
    msj[i]<-"Peligro - Es probable que se produzcan calambres por calor o agotamiento por calor, y es posible que se produzca un golpe de calor con exposicion prolongada y/o actividad fisica."
  }else if (HI[i]>=125){
    msj[i]<-"Extremo Peligro - Golpe de calor muy probable"
  }else if (HI[i]<80){
    msj[i]<-"No hay peligro! :)"
  }
}
for (i in 1:5){
  cat("El dia",i,"se encuentra en la categoria:",msj[i],"\n")
}
#fin :)
