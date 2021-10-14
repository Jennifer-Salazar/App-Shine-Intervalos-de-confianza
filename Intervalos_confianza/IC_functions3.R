
normalidad <-  TRUE

muestra <- TRUE

conocida <- TRUE

# pivote media ------------------------------------------------------------

ic_pivote_media <- function(x_barra, desv, n, alpha, conocida, normalidad){
  
  if(normalidad & !conocida){
    
    pivote_li <- x_barra - qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    pivote_ls <- x_barra + qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }else if(!normalidad & n<=30){
   
    intervalo <- "No se puede calcular, es necesario conocer la distribución exacta"
     
  }else{
    
    pivote_li <- x_barra - qnorm(1 - alpha/2) *  desv/sqrt(n)
    pivote_ls <- x_barra + qnorm(1 - alpha/2) *  desv/sqrt(n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }
  
  return(intervalo)
  
}


# Boostrap media ----------------------------------------------------------


ic_boostrap_media <- function(variable, alpha){
  
  library(boot)
  
  media <- function(variable, muestra){
    
    d <- variable[muestra]
    
    return(mean(d))
  }
  
  
  replicas <- boot(data = variable, statistic = media, R = 5000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  
  return(ic_boostrap)

}


# Máxima verosimilitud media ----------------------------------------------

ic_mv_media <- function(x_barra, desv, n, alpha){
  
  mv_funcion <- function(x){
    
    p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
    
    return(-n/(2*desv^2) * (x - x_barra)^2 - log(p))
  }
  
  mv_li <- uniroot(mv_funcion, c(-999999, x_barra), tol = 0.000001)$root
  mv_ls <- uniroot(mv_funcion, c(x_barra, 999999), tol = 0.000001)$root
  
  intervalo <- c(mv_li, mv_ls)
  
  return(intervalo)
}
