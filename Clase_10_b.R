###LISTAS
familia <- list("Maria","Juana", 10, c("Hugo", "Paula"),c(8,7))   #ejemplo
familia <- list(madre = "Maria", tia = "Juana", casados = 10,hijos = c("Hugo","Paula"), edades = c(8, 6))

#Ejercicio
lista<-list(alumnxs=c("Cate","Gasti","Lari","Cande","Feli"),edades=c(20,22,21,20,22))
lista$promedio<-mean(lista[[2]])
lapply(lista,function(x)length(x))
