# Se cargan los datos
data <- read.csv("www/Ejercicio6.txt", header = TRUE, encoding = "UTF-8")

variable <- data$Precio

varianza <- var(variable)

alpha <-  0.05



# IC para la media con varianza conocida ----------------------------------

ic_media_conocida <- function(variable, varianza, alpha){
  
  x_barra <- mean(variable)
  desv <- varianza^0.5
  n <- length(variable)
  
  # Método del pivote -------------------------------------------------------
  
  pivote_li <- x_barra - qnorm(1 - alpha/2) *  desv/sqrt(n)
  pivote_ls <- x_barra + qnorm(1 - alpha/2) *  desv/sqrt(n)
  
  ic_pivote <- c(pivote_li, pivote_ls)
  
  # Máxima verosimilitud ----------------------------------------------------
  
  mv_funcion <- function(x){
    
    p <- exp(-1 * qchisq(p = 1 - alpha, df = 1)/2)
    
    return(-n/(2*desv^2) * (x - x_barra)^2 - log(p))
  }
  
  mv_li <- uniroot(mv_funcion, c(-999999, x_barra), tol = 0.000001)$root
  mv_ls <- uniroot(mv_funcion, c(x_barra, 999999), tol = 0.000001)$root
  
  ic_mv <- c(mv_li, mv_ls)
  
  # boostrap ----------------------------------------------------------------
  
  library(boot)
  
  media <- function(datos, muestra){
    
    d <- datos[muestra]
    
    return(mean(d))
  }
  
  
  replicas <- boot(data = variable, statistic = media, R = 5000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  
  # return ------------------------------------------------------------------

  metodos <- c("Método del pivote", "Máxima verosimilitud", "Boostrap BCA")
  
  intervalos <- cbind(ic_pivote, ic_mv, ic_boostrap)
  colnames(intervalos) <- metodos
  row.names(intervalos) <- c("límite inferior", "límite superior")
  
  
  return(intervalos)
  
}

 

# IC para la media con varianza desconocida -------------------------------

ic_media_desconocida <- function(variable, alpha, normalidad){
 
  x_barra <- mean(variable)
  desv <- sd(variable)
  n <- length(variable)
  
  metodo <- ""
  
  if(normalidad == TRUE){
    
    metodo <- "Método del pivote"
    
    # Método del pivote -------------------------------------------------------

    pivote_li <- x_barra - qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    pivote_ls <- x_barra + qt(1 - alpha/2, n-1) *  desv/sqrt(n)
    
    ic_pivote <- c(pivote_li, pivote_ls)

  }else if(n > 30){
    
    metodo <- "Aprox. Teorema del límite central"
    
    # TLC ---------------------------------------------------------------------

    tlc_li <- x_barra - qnorm(1 - alpha/2) *  desv/sqrt(n)
    tlc_ls <- x_barra + qnorm(1 - alpha/2) *  desv/sqrt(n)
    
    ic_pivote <- c(tlc_li, tlc_ls)
  }

  # Boostrap ----------------------------------------------------------------

  library(boot)
  
  media <- function(datos, muestra){
    
    d <- datos[muestra]
    
    return(mean(d))
  }
  
  
  replicas <- boot(data = variable, statistic = media, R = 5000)
  
  ic_boostrap <-  boot.ci(replicas, conf = 1 - alpha, type = "bca")$bca[4:5]
  

  # return ------------------------------------------------------------------

  if(metodo == ""){
    
    intervalos <- as.data.frame(ic_boostrap)
    colnames(intervalos) <- "Boostrap BCA"
    
  }else{
    
    intervalos <- cbind(ic_pivote, ic_boostrap)
    colnames(intervalos) <- c(metodo, "Boostrap BCA")
  }
  
  row.names(intervalos) <- c("límite inferior", "límite superior")
  
  return(intervalos)
    
}






