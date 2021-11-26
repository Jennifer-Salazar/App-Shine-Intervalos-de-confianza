
source("graf_ic_media.R")

# pivote media ------------------------------------------------------------

ic_pivote_media <- function(x_barra, desv, n, alpha, conocida, normalidad){
  

  # Calculo del intervalo ---------------------------------------------------

  
  # Si hay normalidad y la varianza es desconocida
  
  if(normalidad & !conocida){
    
    pivote_li <- x_barra - qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    pivote_ls <- x_barra + qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  # Si no hay normalidad o el tamaño de muestra es menor a 30
    
  }else if(!normalidad & n<=30){
   
    intervalo <- "No se puede calcular, es necesario conocer la distribución exacta"
    
  # Cuando hay normalidad y la varianza es conocida o cuando n > 30
     
  }else{
    
    pivote_li <- x_barra - qnorm(1 - alpha/2) *  desv/sqrt(n)
    pivote_ls <- x_barra + qnorm(1 - alpha/2) *  desv/sqrt(n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }
  

  # Obtener gráfica ---------------------------------------------------------

  grafica <- graf_pivote_media(x_barra, desv, pivote_li, pivote_ls, n, conocida, normalidad)
  
  
  return(list(intervalo, grafica))
  
}


# Boostrap media ----------------------------------------------------------


ic_boostrap_media <- function(variable, alpha){
  

  # Calculo del intervalo ---------------------------------------------------
  
  library(boot)
  
  media <- function(variable, muestra){
    
    d <- variable[muestra]
    
    return(mean(d))
  }
  
  
  replicas <- boot(data = variable, statistic = media, R = 100000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  
  # Obtener gráfica ---------------------------------------------------------
  
  boostrap_li <- ic_boostrap[1]
  boostrap_ls <- ic_boostrap[2]
  
  grafica <- graf_boostrap_media(replicas, x_barra = mean(variable), boostrap_li, boostrap_ls)
  
  
  return(list(ic_boostrap, grafica))

}


# Máxima verosimilitud media ----------------------------------------------

ic_mv_media <- function(x_barra, desv, n, alpha, conocida){
  
  # Calculo del intervalo ---------------------------------------------------
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  
  # Cuando la varianza es conocida
  
  if(conocida){

    mv_funcion <- function(mu, p){
      
      return(-n/(2*desv^2) * (mu - x_barra)^2 - log(p))
    }
    
    mv_li <- uniroot(mv_funcion, c(-999999, x_barra), tol = 0.000001, p = p)$root
    mv_ls <- uniroot(mv_funcion, c(x_barra, 999999), tol = 0.000001, p = p)$root
    
  # Cuando la varianza es desconocida
  
  }else{
    
    var_estimada <- (n-1)/n * desv^2
    
    rmax_mu <- function(mu, p){
      
      return(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2)) - log(p))
      
    }
    
    mv_li <- uniroot(rmax_mu, c(-999999, x_barra), tol = 0.000001, p = p)$root
    mv_ls <- uniroot(rmax_mu, c(x_barra, 999999), tol = 0.000001, p = p)$root
    
  }
  
  intervalo <- c(mv_li, mv_ls)
  
  # Obtener gráfica ---------------------------------------------------------
  
  grafica <- graf_mv_media(x_barra, desv, n, conocida, mv_li, mv_ls, p, alpha)
  
  return(list(intervalo, grafica))
}
