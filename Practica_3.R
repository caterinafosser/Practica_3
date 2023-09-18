###PRACTICA 3###
#Ejercicio 1----
#a
vector_nros=c()
for (i in 1:1000){
  vector_nros[i]=i
}
#b
matriz_a=matrix(nrow=30,ncol=20)
for (i in 1:nrow(matriz_a)){
  for (j in 1:ncol(matriz_a)){
    matriz_a[i,j]<-i*j
  }
}
#c
matriz_fil=matrix(nrow=30,ncol=20)
for (i in 1:nrow(matriz_fil)){
  for (j in 1:ncol(matriz_fil)){
    matriz_fil[i,j]<-i
  }
}
matriz_col=matrix(nrow=30,ncol=20)
for (i in 1:nrow(matriz_col)){
  for (j in 1:ncol(matriz_col)){
    matriz_col[i,j]<-j
  }
}
matriz_b=matriz_fil*matriz_col
#d
quinta_columna=matriz_b[,5]
#e
filas=20
columnas=30
tiempos=10
array=array(dim=c(filas,columnas,tiempos)
for (i in 1:filas){
  for (j in 1:columnas){
    for (k in 1:tiempos){
      array[i,j,k]=i*j*k
    }
  }
}
tiempo_4=array[,,4]

#Ejercicio 2----
#a
a=c(4,-10,7,-2,8,-6,1,-15,3,-9)
i=3
j=5
b=c(1,5,7)
c=c(T,T,F,T,F,F,T,F,F,F)

a[]   #da todos los numeros del vecotr
a[i,j]  #error pq no es una matriz
a[seq(i,i,2)] 
a[seq(i,j,2)]  #las secuencias las lee bien (SEQ=(INICIO,FIN,BY))
a[i:length(a)]
a[c(1,5,7)]  #los vectores tambien
a[b]
a[c]
a[a==-4]  #esto? q onda? loco q me de la clase

#b
mi.matriz=matrix(1:25,ncol=5,byrow=T)
mi.matriz[seq(1,5,2),] #entre las filas 1 y 5 cada 2
mi.matriz[,seq(2,5,2)] #entre las columnas 2 y 5 cada 2
mi.matriz[seq(1,5,2),seq(1,5,2)]
mi.matriz[10]
mi.matriz[b]

#Ejercicio 3----
#a
#ES MUY DIFICIL AAAAA
A=matrix(1:8,nrow=2)
B=matrix(9:16,nrow=4)
C=matrix(0,nrow=nrow(A),ncol=ncol(B))
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    C[i,j]=C[i,j]+A[i,j]*B[j,i]
  }
}

#b
A=matrix(1:2000,ncol=100)
B=matrix(2001:4000,ncol=100)
AB=matrix(0,ncol=ncol(A),nrow=nrow(A))
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    AB[i,j]=A[i,j]*B[i,j]
  }
}
