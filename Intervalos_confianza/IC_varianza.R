

# pivote varianza ---------------------------------------------------------

ic_pivote_varianza <- function(s2, n, alpha, conocida, normalidad){
  
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

ic_mv_varianza_conocida <- function(s2_x_barra, s2_mu, mu, n, alpha){
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  # Cuando la media es conocida
  
  rmax_sigma <- function(varianza, p){
    
    return(n/2 * ( log(s2_x_barra/varianza) + s2_mu*((1/s2_x_barra)-(1/varianza))) - log(p))
    
  }
  
  mv_li <- uniroot(rmax_sigma, c(0.00001, s2_mu), tol = 0.000001, p = p)$root
  mv_ls <- uniroot(rmax_sigma, c(s2_mu, 999999), tol = 0.000001, p = p)$root
  
  
  
  intervalo <- c(mv_li, mv_ls)
  
  return(intervalo)
}
    
  
    
ic_mv_varianza_desconocida <- function(s2, mu, n, alpha){
  
  p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
  
  var_estimada <- (n-1)/n * s2
  
  rmax_sigma <- function(varianza, p){
    
    return(n/2 * ( log(var_estimada/varianza) - var_estimada/varianza + 1) - log(p))
    
  }
  
  mv_li <- uniroot(rmax_sigma, c(0.00001, var_estimada), tol = 0.000001, p = p)$root
  mv_ls <- uniroot(rmax_sigma, c(var_estimada, 999999), tol = 0.000001, p = p)$root
  
  intervalo <- c(mv_li, mv_ls)
  
  return(intervalo)
  
}
    
    
  
  
 




