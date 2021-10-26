

# pivote varianza ---------------------------------------------------------

ic_pivote_varianza <- function(s, n, alpha, conocida, normalidad){
  
  if(normalidad & !conocida){
    
    # No conocida
    
    pivote_li <- (n-1) * s / qchisq(1 - alpha/2, n-1)
    pivote_ls <- (n-1) * s / qchisq(alpha/2, n-1)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }else if(!normalidad & n<=30){
    
    intervalo <- "No se puede calcular, es necesario conocer la distribución exacta"
    
  }else{
    
    # Conocida
    
    pivote_li <- n * s / qchisq(1 - alpha/2, n)
    pivote_ls <- n * s / qchisq(alpha/2, n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }
  
  return(intervalo)
  
}


# Boostrap varianza -------------------------------------------------------


ic_boostrap_varianza <- function(variable, alpha){
  
  library(boot)
  
  varianza <- function(variable, muestra){
    
    d <- variable[muestra]
    
    return(var(d))
  }
  
  
  replicas <- boot(data = variable, statistic = varianza, R = 5000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  
  return(ic_boostrap)
  
}

# Máxima verosimilitud varianza -------------------------------------------


