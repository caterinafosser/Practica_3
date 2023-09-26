###FUNCIONES
nudos_a_ms <- function(viento) {
  if (!is.numeric(viento)) stop("Viento no es numerico")
  ms <- viento * 0.5144
  return(ms)
}
#Ejercicio
ms_a_nudos<-function(viento){
  nudos<-viento*1.944
  return(nudos)
}

datos_viento <- data.frame(hora = seq(0, 9),
                           viento = c(21.58, 18.08, 7.19, 7.19, 7.19, 7.19, 7.19,
                                      3.69, 3.69, 7.19))
datos_viento$viento_ms <- nudos_a_ms(datos_viento$viento)
head(datos_viento)


nudos_a_ms <- function(viento, conversion = 0.51) {    ##es una constante flexible, si no se ingresa otro valor anda piola pero si necesito mas precision x ej se puede modificar
  if (!is.numeric(viento)) stop("viento no es numérico")
  ms <- viento * conversion
  return(ms)
}

conversion_viento<-function(viento,unidades){
  if (!is.numeric(viento)) stop("Viento no es numerico")
  if (unidades=="ms"){
    ms <- viento * 0.5144
    return(ms)
    } else if (unidades=="nudos"){
    nudos<-viento*1.944
    return(nudos)
  }else{
    cat("Error. ingrese una unidad valida ('ms' o 'nudos')") 
  }
}
