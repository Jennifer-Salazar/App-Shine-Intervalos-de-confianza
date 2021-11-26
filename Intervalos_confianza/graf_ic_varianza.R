

# Gráficos privote varianza -----------------------------------------------

graf_pivote_varianza <- function(s2, n, conocida, alpha, li, ls){
  
  
  if(conocida){
    grados_libertad <- n
  }else{
    grados_libertad <- n-1
  }
  
  chi2 <- rchisq(100000, grados_libertad)
  distribucion <- (grados_libertad*s2)/chi2
  
  # valores minimos y máximos para el gráfico de la normal
  
  # Se obtiene el eje x y el eje y
  eje_x <- density(distribucion)$x
  eje_y <- density(distribucion)$y
  
  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, eje_x[eje_x >= li & eje_x <= ls], ls)
  y_ic <- c(0, eje_y[eje_x >= li & eje_x <= ls], 0) 
  
  # Gráfico
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xlim = c(0, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2, lty=3)
  #axis(1, at = round(c(li, s2, ls), 3), las =2)
  title(main = "Método de Pivote")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
}



# Gráficos boostrap varianza ----------------------------------------------

graf_boostrap_varianza <- function(replicas, s2, li, ls){
  
  # Se obtiene el eje x y el eje y
  eje_x <- density(replicas$t)$x
  eje_y <- density(replicas$t)$y
  
  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, eje_x[eje_x >= li & eje_x <= ls], ls)
  y_ic <- c(0, eje_y[eje_x >= li & eje_x <= ls], 0) 
  
  # Gráfico
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xlim = c(0, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2, lty=3)
  #axis(1, at = round(c(li, s2, ls), 3))
  title(main = "Método de Bootstrap")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
  
}


# Gráficos MV varianza conocida -------------------------------------------


graf_mv_varianza_conocida <- function(s2_x_barra, s2_mu, n, li, ls, p, alpha){
  
  # Función de verosimilitud relativa
  
  R_varianza <- function(varianza){
    
    return(exp(n/2 * ( log(s2_x_barra/varianza) + s2_mu*((1/s2_x_barra)-(1/varianza))))) 
    
  }
  
  # valores minimos y máximos para el gráfico
  x_min <- 0
  x_max <- ls + (ls-li)/4

  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)

  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_varianza(seq(li, ls, by=(ls-li)/1000)), 0)

  # Gráfico
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xlim = c(0, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2_mu, lty=3)
  abline(h = p, lty=3)
  text(li-sqrt(s2_mu)/2,p+0.05, labels = paste((1-alpha)*100, "%", sep=""))
  #axis(1, at = round(c(li, s2_mu, ls), 3))
  title(main = "Método de Máxima Verosimilitud")
  abline(h = 0)
  grid()

  grafico <- recordPlot()

  return(grafico)
}


# Gráficos MV varianza desconocida ----------------------------------------

graf_mv_varianza_desconocida <- function(s2, n, li, ls, p, alpha){
  
  var_estimada <- (n-1)/n * s2
  
  # Función de verosimilitud relativa
  
  R_varianza <- function(varianza){
    
    return(exp(n/2 * ( log(var_estimada/varianza) - var_estimada/varianza + 1)))
    
  }
  
  # valores minimos y máximos para el gráfico
  x_min <- 0
  x_max <- ls + (ls-li)/4
 
  # Se obtiene el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_varianza(eje_x)
  
  # Se crean los puntos para pintar el área del intervalo
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_varianza(seq(li, ls, by=(ls-li)/1000)), 0)
  
  # Gráfico
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xlim = c(0, ls + (ls-li)/4))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2, lty=3)
  abline(h = p, lty=3)
  text(li-sqrt(s2)/2,p+0.05, labels = paste((1-alpha)*100, "%", sep=""))
  #axis(1, at = round(c(li, s2, ls), 3))
  title(main = "Método de Máxima Verosimilitud")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
}
