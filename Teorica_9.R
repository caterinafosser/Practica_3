print(iris)
plot(iris)
summary(iris)
str(iris)

m<-cbind(ord=1:3,edad=c(30L,26L,9L))
v<-c(1.80,1.72,1.05)
ff<-data.frame(familia=c("Padre","Madre","Hijo"),m,estatura=v)
ff<-data.frame(familia=c("Padre","Madre","Hijo"),m,estatura=v,row.names=1)


mitabla<-read.table("Leer_tabla.txt")
is.data.frame(mitabla)
colnames(mitabla)
rownames(mitabla)

mitabla$Piso
mitabla[[2]] #es un vector, ==mitabla[,2]
mitabla[2]  #es un data.frame
mitabla[3,2]<-106 #modifico un valor
mitabla$Total<-mitabla$Precio*mitabla$Area #genera una nueva columna

datosimp <- data.frame(anyos=c(1.3,0.4,1.1,2.3,3.1,1.3),
                       tipo=c(2,3,3,1,3,1),edad=c(22,21,34,42,17,43),
                       sexo=c("H","M","H","H","M","H"))
attach(datosimp) #me lo guarda y puedo llamar a anyos, tipo, sexo
detach(datosimp) #lo ""borro""

datos.hombre.filtrados <- datosimp$anyos[datosimp$sexo=='H']
mas.peq <- subset(datosimp,anyos<2,select=c(edad,sexo)) #selecciona segun caracteristicas
datosimp<-transform(datosimp,edad=edad+1)
datosimp


aq=airquality
#temperatura media de todos los dias
temp_media<-mean(aq$Temp)
#temperatura media en mayo
temp_media_mayo<-mean(aq$Temp[aq$Month==5])
#dia mas ventoso, dia y mes del viento maximo
cat("El dia mas ventoso fue el",(aq$Day[aq$Wind==max(aq$Wind)]),"del mes",(aq$Month[aq$Wind==max(aq$Wind)]),"con un valor de",max(aq$Wind))
#ordenar info segun velocidad ascendente
aq<-aq[order(aq$Wind),]
#datos de color, sort(aq$Wind) me ordena 
#tabla con variables para mayo
var_mayo<-subset(aq,aq$Month==5,select=c(Ozone,Solar.R))
ozono<-plot(var_mayo$Ozone)
radiacion<-plot(var_mayo$Solar.R)

###para cambiar de formato ancho a largo
library(reshape)
formato_long<-melt(aq,id=c("Month","Day"))
print(formato_long)
