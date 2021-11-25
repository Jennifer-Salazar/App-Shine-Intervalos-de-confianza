

# Gráficos privote varianza -----------------------------------------------

graf_pivote_varianza <- function(s2, n, conocida, alpha, li, ls){
  
  if(conocida){
    grados_libertad <- n
  }else{
    grados_libertad <- n-1
  }
  
  
  
  # valores minimos y máximos para el gráfico de la normal
  x_min <- qchisq(0.0001, grados_libertad)
  x_max <- qchisq(0.9999, grados_libertad)
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- dchisq(eje_x, grados_libertad)
  
  # Se crean los puntos para pintar el área del intervalo
  total <- x_max - x_min
  area <- total*(alpha/2)
  li_aux <- x_min + area
  ls_aux <- x_max - area  
  
  s2_aux <- s2 / ((ls-li)/(ls_aux-li_aux))
  s2_aux <- round(s2_aux,3)
  

  x_ic <- c(li_aux, seq(li_aux, ls_aux, by=(ls_aux-li_aux)/1000), ls_aux)
  y_ic <- c(0, dchisq(seq(li_aux, ls_aux, by=(ls_aux-li_aux)/1000), grados_libertad), 0)
  
  # Gráfico
  plot(eje_x, eje_y, type="l", xlab = "", ylab = "", las = 1, lwd = 2, xaxt = "n", bty = "n")
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2_aux, lty=3)
  axis(1, at = c(li_aux, s2_aux, ls_aux), labels = c(li, round(s2,3), ls))
  title(main = "Método del pivote")
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
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, xaxt="n", bty = "n")
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2, lty=3)
  axis(1, at = round(c(li, s2, ls), 3))
  title(main = "Método de Bootstrap")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
  
}


# Gráficos MV varianza conocida -------------------------------------------


graf_mv_varianza_conocida <- function(s2_x_barra, s2_mu, n, li, ls){
  
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
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xaxt="n")
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2_mu, lty=3)
  axis(1, at = round(c(li, s2_mu, ls), 3))
  title(main = "Método de Máxima Verosimilitud")
  abline(h = 0)
  grid()

  grafico <- recordPlot()

  return(grafico)
}


# Gráficos MV varianza desconocida ----------------------------------------

graf_mv_varianza_desconocida <- function(s2, n, li, ls){
  
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
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xaxt="n")
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=s2, lty=3)
  axis(1, at = round(c(li, s2, ls), 3))
  title(main = "Método de Máxima Verosimilitud")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
}
