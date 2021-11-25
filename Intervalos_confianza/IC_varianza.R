source("graf_ic_varianza.R")

# pivote varianza ---------------------------------------------------------

ic_pivote_varianza <- function(s2, n, alpha, conocida, normalidad){
  
  # Calculo del intervalo ---------------------------------------------------
  
  if(normalidad & !conocida){
    
    # No conocida
    
    pivote_li <- (n-1) * s2 / qchisq(1 - alpha/2, n-1)
    pivote_ls <- (n-1) * s2 / qchisq(alpha/2, n-1)
    
    intervalo <- c(pivote_li, pivote_ls)
    
  }else if(normalidad & conocida){
    
    # Conocida
    
    pivote_li <- n * s2 / qchisq(1 - alpha/2, n)
    pivote_ls <- n * s2 / qchisq(alpha/2, n)
    
    intervalo <- c(pivote_li, pivote_ls)
    
     
  }else{
    
    intervalo <- "No se puede calcular, no se cumple el supuesto de normalidad"
    
  }
  
  # Obtener gráfica ---------------------------------------------------------
  
  
  grafica <- graf_pivote_varianza(s2, n, conocida, alpha, pivote_li, pivote_ls)
  
  return(list(intervalo, grafica))
  
}


# Boostrap varianza -------------------------------------------------------


ic_boostrap_varianza <- function(variable, alpha){
  
  # Calculo del intervalo ---------------------------------------------------
  
  library(boot)
  
  varianza <- function(variable, muestra){
    
    d <- variable[muestra]
    
    return(var(d))
  }
  
  
  replicas <- boot(data = variable, statistic = varianza, R = 5000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  
  # Obtener gráfica ---------------------------------------------------------
  boostrap_li <- ic_boostrap[1]
  boostrap_ls <- ic_boostrap[2]
  
  grafica <- graf_boostrap_varianza(replicas, s2 = var(variable), boostrap_li, boostrap_ls)
  
  
  return(list(ic_boostrap, grafica))
  
}

# Máxima verosimilitud varianza -------------------------------------------


# media conocida ----------------------------------------------------------

ic_mv_varianza_conocida <- function(s2_x_barra, s2_mu, n, alpha){
  
  # Calculo del intervalo ---------------------------------------------------
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  # Cuando la media es conocida
  
  rmax_sigma <- function(varianza, p){
    
    return(n/2 * ( log(s2_x_barra/varianza) + s2_mu*((1/s2_x_barra)-(1/varianza))) - log(p))
    
  }
  
  mv_li <- uniroot(rmax_sigma, c(0.00001, s2_mu), tol = 0.000001, p = p)$root
  mv_ls <- uniroot(rmax_sigma, c(s2_mu, 999999), tol = 0.000001, p = p)$root
  
  intervalo <- c(mv_li, mv_ls)
  
  # Obtener gráfica ---------------------------------------------------------
  
  grafica <- graf_mv_varianza_conocida(s2_x_barra, s2_mu, n, mv_li, mv_ls)
  
  return(list(intervalo, grafica))
}
    

# media desconocida -------------------------------------------------------  

ic_mv_varianza_desconocida <- function(s2, mu, n, alpha){
  
  # Calculo del intervalo ---------------------------------------------------
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  var_estimada <- (n-1)/n * s2
  
  rmax_sigma <- function(varianza, p){
    
    return(n/2 * ( log(var_estimada/varianza) - var_estimada/varianza + 1) - log(p))
    
  }
  
  mv_li <- uniroot(rmax_sigma, c(0.00001, var_estimada), tol = 0.000001, p = p)$root
  mv_ls <- uniroot(rmax_sigma, c(var_estimada, 999999), tol = 0.000001, p = p)$root
  
  intervalo <- c(mv_li, mv_ls)
  
  # Calculo del intervalo ---------------------------------------------------
  
  grafica <- graf_mv_varianza_desconocida(s2, n, mv_li, mv_ls)
  
  return(list(intervalo, grafica))
  
}
    
    
  
  
 




