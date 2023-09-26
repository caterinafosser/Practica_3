##################
##################################################################
# # Laboratorio de Procesamiento de Informaci�n Meteorol�gica/Oceanografica
#
# 2� cuatrimestre 2023
#################################################################################
# Clase Funciones
#################


factorial<-function(x) prod(1:x)


factorial(4)
factorial(8)

#####################
hipotenusa <- function(x, y) {
  sqrt(x^2 + y^2)
}

hipotenusa <- function(x, y) {
  return(sqrt(x^2 + y^2))
}

class(hipotenusa)
hipotenusa(2,3)
hipotenusa(y=3,x=2)

#############################
ff <- function(r) {
  return(pi * r^2)
}

ff(2)

######################
ejem.fun <- function(x, y, label = "nombre de variable"){
  plot(x, y, xlab = label) }

ejem.fun(1:5,c(2,4,6,8,10))



ejem.fun.2<-function(x){
  y<-x^2
  plot(x, y) }

######################

grande<-function(x,y){
  y.g<-y>x
  x[y.g]<-y[y.g]
  x
}


grande(1:5, c(1,6,2,7,3))
grande(x=1:5, y=c(1,6,2,7,3))
grande(y=c(1,6,2,7,3), x=1:5)
grande(1:5, y=c(1,6,2,7,3))

############################
media.total<-function(...) { mean(c(...)) }

############################

media.total<-function(...){
  for (x in list(...)){
    if (!is.numeric(x)) stop("No son numeros")
  }
  mean(c(...))
}

############################

myplot <- function(x,y) {
  if(missing(y)) {
    y <- x
    x <- 1:length(y)
  }
  plot(x,y)
}


myplot(1:20,11:30)
myplot(12:20)

############################

xln <- function(x){
  return(-x * log(x))
}
x <- 1:10000 / 10000

plot(x, xln(x), type = "l", xlab = "", ylab = "",
     main = "Funcion -x * log(x)")


########################

grande<-function(x,y=0*x){	
  y.g<-y>x
  x[y.g]<-y[y.g]
  x
}

grande(c(-12:3))
grande(c(1,5),2:3)

############################
grande<-function(x,y=0*x){	
  if (missing(y)) warning("Estamos comparando con 0")
  y.g<-y>x
  x[y.g]<-y[y.g]
  x
}
grande(-3:3)

############################################

DosMuestras <- function(y1, y2) {
  n1 <- length(y1); n2 <- length(y2)
  yb1 <- mean(y1); yb2 <- mean(y2)
  s1 <- var(y1); s2 <- var(y2)
  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  tst <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
  tst
}

altura.hombres<-(c(1.78,1.65,1.82,1.69,1.74,1.89,1.67,1.58,1.72,1.64,1.85,1.92,1.70,1.63))
altura.mujeres<-(c(1.56,1.74,1.63,1.58,1.54,1.72,1.63,1.71,1.65))
DosMuestras(altura.hombres,altura.mujeres)

###########################################

fibbonacci <- function(y) {if (y==0 | y==1)     
  return (1)
  F0 <- 1; F1 <- 1; i <- 2
  repeat {
    s <- F0 + F1                 	
    if (i == y)                    
      return (s)              	
    
    F0 <- F1
    F1 <- s
    i <- i+1                           
  } } 


fibbonacci(0) 
fibbonacci(4) 
fibbonacci(8)                  
fibbonacci(1)
