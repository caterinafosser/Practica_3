a<-list("Nombres"=c("Carlos","Ana","Julieta","Pedro"),"Parentesco"=c("Padre","Madre","Hija","Perro"),"Edades"=c(60,55,8,NA))


x<--4
if (x<0){
  print("Negativo")
}else{
  print("Positivo")
}

iniciales<-c("a","b","c","r","h","w")

for (x in iniciales){
  if (x=="r"){
    break
  }
  print(x)
}

temperatura<-19
while (temperatura>4){
  if (temperatura==14){
    break
  }
  print(temperatura)
}


D<-c("BsAs Salta Jujuy Neuquen Chaco")
E<-c("2","1","15","7")
paste(substr(D,6,9),E[1])
D==E[3]
as.numeric(E[2])/as.numeric(E[1])
sub("Chaco",E[3],D)

a<-readline("Ingrese un valor para a")
x<-as.numeric(a)
i<-1
while(abs(x-(x-((x**2-x)/(2*x))))>10e-4){
  x<-x-((x**2-x)/(2*x))
  i<-i+1
    if (i>10000){
      print("El metodo no converge")
    }else{
      print(paste("La raiz cuadrada de a es",x))
    }
}
}


A<-matrix(c(4,0,-4,1,2,3,8,5,-1,2,0,8),ncol=3)
B<-c(9,19,29)

matrix(B,nrow=3,ncol=4) #a

matrix(A,ncol=2)

a<-A
a[c(2,3,4),1]<-B


A[which(A==8)]<-NA


nombres<-c("Cate","Dali","Male")
anio<-c(2002,2004,2010)
es_arg<-c(FALSE,TRUE,TRUE)

edades<-2023-anio
for (i in 1:length(nombres)){
  if (edades[i]>=18&es_arg[i]==T){
    cat(nombres[i],"esta registradx en el padron")
  }else if (edades[i]<18){
    if (es_arg[i]==T){
      cat(nombres[i],"es menor de edad")
    }else if (es_arg[i]==F){
      cat(nombres[i],"es menor de edad y extranjerx")
    }
  }else if (es_arg[i]==F){
    cat(nombres[i],"es extranjerx")
  }
}


LE<-c("Aeroparque","Macachin","Mar del Plata")
altura<-c(6,231,8)
ciclo_anual_T<-matrix(rnorm(36,mean=25,sd=4),nrow=3)

amplitud<-c()
for (i in 1:length(LE)){
  amplitud[i]<-max(ciclo_anual_T[i,])-min(ciclo_anual_T[i,])
}
datos<-data.frame(LE,altura,amplitud)
media<-apply(ciclo_anual_T,1,mean)
sd<-apply(ciclo_anual_T,1,sd)
datos$media<-media
datos$desvio<-sd


funcion<-function(x){
  f<-2*x**2-0.9*x-1
  return (f)
}



lat<-seq(-90,90,0.5)
lon<-seq(0,357.5,0.5)
anios<-1981:2020
meses<-1:12
datos<-rnorm(tiempo*length(lon)*length(lat),mean=150,sd=130)
datos_anuales<-array(datos,dim=c(length(lon),length(lat),length(anios),length(meses)))

#datos de febreros mes 2
precip_feb<-datos_anuales[,,,2]
precip_feb_cordoba<-precip_feb[-31.5,244,]
