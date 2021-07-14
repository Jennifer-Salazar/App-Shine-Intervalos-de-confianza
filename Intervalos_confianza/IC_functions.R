
# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")

variable <- data$Precio

# function(variable, parametro, metodo, alpha, n, conocida = TRUE, normalidad = TRUE)

media <- mean(variable)

desv <- sd(variable)

n <- length(variable)

alpha <-  0.05

# I.C para la media -------------------------------------------------------

if(parametro == "media"){
  
  if(normalidad == TRUE){
    
    # Método del pivote -------------------------------------------------------
    
    if(conocida == TRUE){

      # varianza conocida
      
      li <- media - qnorm(1 - alpha/2) *  desv/sqrt(n)
      ls <- media + qnorm(1 - alpha/2) *  desv/sqrt(n)
      
    }else{
    
      # varianza desconocida
      
      li <- media - qt(1 - alpha/2, n-1) *  desv/sqrt(n)
      ls <- media + qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    }
    
  }else if(n > 30){
    
    # Teorema del límite central ----------------------------------------------
    
    li <- media - qnorm(1 - alpha/2) *  desv/sqrt(n)
    ls <- media + qnorm(1 - alpha/2) *  desv/sqrt(n)
    
  }else{
    
    paste("se jodio")
    
  }
}