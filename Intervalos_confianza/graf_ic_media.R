
# Gráficos pivote media ---------------------------------------------------

graf_pivote_media <- function(x_barra, desv, li, ls, n, conocida, normalidad){
  
  # Si hay normalidad y la varianza es desconocida
  
  if(normalidad & !conocida){
    
    T_student <- rt(100000, n-1)
    distribucion <- x_barra -  (T_student * desv/sqrt(n))
    
    # valores minimos y máximos para el gráfico de la normal
    
    # Se obtiene el eje x y el eje y
    eje_x <- density(distribucion)$x
    eje_y <- density(distribucion)$y
    
    # Se crean los puntos para pintar el área del intervalo
    x_ic <- c(li, eje_x[eje_x >= li & eje_x <= ls], ls)
    y_ic <- c(0, eje_y[eje_x >= li & eje_x <= ls], 0) 
    
    # Gráfico
    plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n")
    polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
    abline(v=x_barra, lty=3)
    #axis(1, at = round(c(li, x_barra, ls), 3), las =2)
    title(main = "Método de Pivote")
    abline(h = 0)
    grid()
    
    
    grafico <- recordPlot()
  
  # Si no hay normalidad o el tamaño de muestra es menor a 30
  
  }else if(!normalidad & n<=30){
    
    plot.new()
    
    grafico <- recordPlot()
    
    # Cuando hay normalidad y la varianza es conocida o cuando n > 30
    
  }else{
    
    
    
    Normal <- rnorm(100000)
    distribucion <- x_barra -  (Normal * desv/sqrt(n))
    
    # valores minimos y máximos para el gráfico de la normal
    
    # Se obtiene el eje x y el eje y
    eje_x <- density(distribucion)$x
    eje_y <- density(distribucion)$y
    
    # Se crean los puntos para pintar el área del intervalo
    x_ic <- c(li, eje_x[eje_x >= li & eje_x <= ls], ls)
    y_ic <- c(0, eje_y[eje_x >= li & eje_x <= ls], 0) 
    
    # Gráfico
    plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n")
    polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
    abline(v=x_barra, lty=3)
    #axis(1, at = round(c(li, x_barra, ls), 3), las =2)
    title(main = "Método de Pivote")
    abline(h = 0)
    grid()
    
    # # valores minimos y máximos para el gráfico de la normal
    # x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
    # x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
    # 
    # # Se simula el eje x y el eje y
    # eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
    # eje_y <- dnorm(eje_x, mean = x_barra, sd = desv)
    # 
    # # Se crean los puntos para pintar el área del intervalo
    # x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
    # y_ic <- c(0, dnorm(seq(li, ls, by=(ls-li)/1000), mean = x_barra, sd = desv), 0)
    # 
    # # Gráfico
    # plot(eje_x, eje_y, type="l", xlab = "", ylab = "", las = 1, lwd = 2, bty = "n", xlim=c(x_barra-3*desv, x_barra+3*desv))
    # polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
    # polygon(rep(x_barra, 2), c(0, dnorm(x_barra, mean = x_barra, sd = desv)), lty=3)
    # #axis(1, at = c(li, x_barra, ls))
    # title(main = "Método del pivote")
    # abline(h = 0)
    # grid()
    
    grafico <- recordPlot()
    
  }
  
  return(grafico)
  
}


# Gráficos boostrap media -------------------------------------------------


graf_boostrap_media <- function(replicas, x_barra, li, ls){
  
  eje_x <- density(replicas$t)$x
  eje_y <- density(replicas$t)$y
  
  x_ic <- c(li, eje_x[eje_x >= li & eje_x <= ls], ls)
  y_ic <- c(0, eje_y[eje_x >= li & eje_x <= ls], 0) 
  
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n")
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=x_barra, lty=3)
  #axis(1, at = round(c(li, x_barra, ls), 3))
  title(main = "Método de Bootstrap")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
  
}


# Gráfico MV media --------------------------------------------------------


graf_mv_media <- function(x_barra, desv, n, conocida, li, ls){
  
  # Cuando la varianza es conocida
  
  if(conocida){
    
    R_mu <- function(mu){
      
      return(exp(-n/(2*desv^2) * (mu - x_barra)^2))
    }
    
  }else{
    var_estimada <- (n-1)/n * desv^2
    
    R_mu <- function(mu){
      
      return(exp(n/2 * log(var_estimada/(var_estimada + (x_barra - mu)^2))))
      
    }
  }
  
  
  # valores minimos y máximos para el gráfico de la normal
  x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
  x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
  
  
  # Se simula el eje x y el eje y
  eje_x <- seq(x_min, x_max, by = (x_max-x_min)/1000)
  eje_y <- R_mu(eje_x)
  
  x_ic <- c(li, seq(li, ls, by=(ls-li)/1000), ls)
  y_ic <- c(0, R_mu(seq(li, ls, by=(ls-li)/1000)), 0) 
  
  plot(eje_x, eje_y, ylab="", xlab="", type="l", lwd=2, bty = "n", xlim= c(x_barra-2*desv, x_barra+2*desv))
  polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
  abline(v=x_barra, lty=3)
  #axis(1, at = round(c(li, x_barra, ls), 3))
  title(main = "Método de Máxima Verosimilitud")
  abline(h = 0)
  grid()
  
  grafico <- recordPlot()
  
  return(grafico)
}

  
  
  
  