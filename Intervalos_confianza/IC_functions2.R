# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")

variable <- data$Precio


#ic_mu <- 
  
#ic_varianza <- 

#ic_dif <- 

# Método del pivote -------------------------------------------------------

ic_pivote <- function(parametro, conocida, alpha = 0.05, n, mu, desv){
  
  if(parametro == "media"){
    
    # varianza conocida
    
    if(conocida == TRUE){
      
      li <- mu - qnorm(1 - alpha/2) *  desv/sqrt(n)
      ls <- mu + qnorm(1 - alpha/2) *  desv/sqrt(n)
      
    # varianza desconocida

    }else{
      
      li <- mu - qt(1 - alpha/2, n-1) *  desv/sqrt(n)
      ls <- mu + qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    }
    
  }
  
  return(li, ls)
  
}



# Teorema del límite central ----------------------------------------------

ic_tlc <- function(parametro, conocida, alpha = 0.05, n, mu, desv){
  
  if(parametro == "media"){

    if(n > 30){
      
      li <- mu - qnorm(1 - alpha/2) *  desv/sqrt(n)
      ls <- mu + qnorm(1 - alpha/2) *  desv/sqrt(n)
      
    }else{
      
      paste("se debe de conocer la distribución exacta")
      
    }
    
  }else{
    
  }
  
  return(li, ls)
}