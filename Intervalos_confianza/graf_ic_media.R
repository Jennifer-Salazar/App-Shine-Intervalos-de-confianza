
require(latex2exp)

# Gráficos pivote ---------------------------------------------------------

ic_pivote_media <- function(x_barra, desv, li, ls, n, conocida, normalidad){
  
  # Si hay normalidad y la varianza es desconocida
  
  if(normalidad & !conocida){
    
    li <- 26
    ls <- 32
    
    # valores minimos y máximos para el gráfico de la normal
    x_min <- qt(0.0001, df = n-1)
    x_max <- qt(0.9999, df = n-1)
    
    # Se simula el eje x y el eje y
    eje_x <- seq(x_min, x_max, by = .1)
    eje_y <- dt(eje_x, df = n-1)
    
    # Se crean los puntos para pintar el área del intervalo
    
    x <- seq(li, ls)
    y <- dt((x-x_barra)/desv, n-1)
    x_ic <- c(li, x, ls)
    y_ic <- c(0, y, 0)
    

    
    # Gráfico
    plot(eje_x + x_barra, eje_y, type="l", xlab = "", ylab = "", las = 1, lwd = 2, bty = "n")
    polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
    #polygon(rep(x_barra, 2), c(0, dt(x_barra, df = n-1)), lty=3)
    axis(1, at = c(li, x_barra, ls))
    title(main = "Método del pivote")
    abline(h = 0)
  
  
  # Si no hay normalidad o el tamaño de muestra es menor a 30
  
  }else if(!normalidad & n<=30){
    
    intervalo <- "No se puede calcular, es necesario conocer la distribución exacta"
    
    # Cuando hay normalidad y la varianza es conocida o cuando n > 30
    
  }else{
    
    # valores minimos y máximos para el gráfico de la normal
    x_min <- qnorm(0.0001, mean = x_barra, sd = desv)
    x_max <- qnorm(0.9999, mean = x_barra, sd = desv)
    
    # Se simula el eje x y el eje y
    eje_x <- seq(x_min, x_max, by = .1)
    eje_y <- dnorm(eje_x, mean = x_barra, sd = desv)
    
    # Se crean los puntos para pintar el área del intervalo
    x_ic <- c(li, seq(li, ls), ls)
    y_ic <- c(0, dnorm(li:ls, mean = x_barra, sd = desv), 0)
    
    # Gráfico
    plot(eje_x, eje_y, type="l", xlab = "", ylab = "", las = 1, lwd = 2, xaxt = "n", bty = "n")
    polygon(x_ic, y_ic, col = rgb(1, 0, 0, alpha = 0.5))
    polygon(rep(x_barra, 2), c(0, dnorm(x_barra, mean = x_barra, sd = desv)), lty=3)
    axis(1, at = c(li, x_barra, ls))
    title(main = "Método del pivote")
    abline(h = 0)
    
    
  }

  
}