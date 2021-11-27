
# latex pivote media ---------------------------------------------------

latex_pivote_media <- function(conocida, normalidad, n){
  

  if(normalidad & !conocida){
    
    texto <- paste("$$\\overline{X} \\pm t_{\\frac{\\alpha}{2}, (n-1)} \\times \\frac{s}{\\sqrt{n}}$$ ")
    

  }else if(!normalidad & n<=30){
    
    texto <- paste("$$ $$")
    
  }else if(conocida){
    
  texto <- paste("$$\\overline{X} \\pm Z_{\\frac{\\alpha}{2}} \\times \\frac{\\sigma}{\\sqrt{n}}$$ ")
  }
  
  else{
    
    texto <- paste("$$\\overline{X} \\pm Z_{\\frac{\\alpha}{2}} \\times \\frac{s}{\\sqrt{n}}$$ ")
  
  }
  
  return(texto)
  
}


# latex boostrap media -------------------------------------------------


latex_boostrap_media <- function(num_replicas){
  
  texto <- paste("$$\\text{El número de replicas son: }", "100.000","$$")
  
  return(texto)
  
}


# latex MV media --------------------------------------------------------


latex_mv_media <- function(conocida){
  
  texto <- paste("$$\\text{Verosimilitud relativa:   } R(\\theta) = \\frac{L(\\theta)}{L(\\hat{\\theta})} \\\\ \\text{El conjunto de valores } \\theta \\text{ para los cuales } R(\\theta) \\geq p \\\\ \\text{es llamado el intervalo de verosimilitud}  
\\\\ \\text{Los limites del intervalo se obtienen al resolver la ecuación:} 
\\\\ R(\\theta) = p 
\\\\ r(\\theta) = Ln(R(\\theta)) = Ln(p)
\\\\ r(\\theta) - Ln(p)=0
\\\\ \\text{donde p esta definido como:  } p = e^{\\frac{-\\chi^2_{1-\\alpha,1}}{2}}$$")
  
  # Cuando la varianza es conocida
  
  if(conocida){
    
    texto2 <- paste("$$r(\\mu)= \\frac{-n}{2 \\sigma^2} (\\mu - \\overline{x})^2 \\\\
 R(\\mu) = e^{\\frac{-n}{2 \\sigma^2} (\\mu - \\overline{x})^2}$$")
    
  }else{
    texto2 <- paste("$$r(\\mu)= \\frac{n}{2} Ln\\left(\\frac{\\hat{\\sigma}^2}{\\hat{\\sigma}^2+(\\overline{x}-\\mu)^2}\\right) \\\\
R(\\mu) = e^{\\frac{n}{2} Ln\\left(\\frac{\\hat{\\sigma}^2}{\\hat{\\sigma}^2+(\\overline{x}-\\mu)^2}\\right)}$$")
  }
  
  
  return(list(texto, texto2))
}


# latex pivote varianza --------------------------------------------------------

latex_pivote_varianza <- function(conocida){
  
  
  if(conocida){
    texto <- paste("$$\\left( \\frac{\\sum_{i=1}^{n}(x_i-\\mu)^2}{\\chi^2_{1-\\frac{\\alpha}{2}(n)}}, \\frac{\\sum_{i=1}^{n}(x_i-\\mu)^2}{\\chi^2_{\\frac{\\alpha}{2}(n)}}\\right)$$")
    
  }else{
    texto <- paste("$$\\left( \\frac{\\sum_{i=1}^{n}(x_i-\\overline{x})^2}{\\chi^2_{1-\\frac{\\alpha}{2}(n-1)}}, \\frac{\\sum_{i=1}^{n}(x_i-\\overline{x})^2}{\\chi^2_{\\frac{\\alpha}{2}(n-1)}}\\right)$$")
  }
  return(texto)
  
}

# latex boostrap media -------------------------------------------------


latex_boostrap_varianza <- function(num_replicas){
  
  texto <- paste("$$\\text{El número de replicas son: }", "100.000", "$$")
  
  return(texto)
  
}


# latex MV varianza -------------------------------------------------


latex_mv_varianza <- function(conocida){
  
  
  texto <- paste("$$\\text{Verosimilitud relativa:   } R(\\theta) = \\frac{L(\\theta)}{L(\\hat{\\theta})} \\\\ \\text{El conjunto de valores } \\theta \\text{ para los cuales } R(\\theta) \\geq p \\\\ \\text{es llamado el intervalo de verosimilitud}  
\\\\ \\text{Los limites del intervalo se obtienen al resolver la ecuación:} 
\\\\ R(\\theta) = p 
\\\\ r(\\theta) = Ln(R(\\theta)) = Ln(p)
\\\\ r(\\theta) - Ln(p)=0
\\\\ \\text{donde p esta definido como:  } p = e^{\\frac{-\\chi^2_{1-\\alpha,1}}{2}}$$")
  
  # Cuando la varianza es conocida
  
  if(conocida){
    
    texto2 <- paste("$$r(\\sigma^2)=\\frac{n}{2} \\left[ Ln \\left( \\frac{\\hat{\\sigma}^2}{\\sigma^2}\\right) + S_{\\mu}^2 \\left(\\frac{1}{\\hat{\\sigma}^2}- \\frac{1}{\\sigma^2}\\right)\\right] \\\\
                    R(\\sigma^2) = e^{\\frac{n}{2} \\left[ Ln \\left( \\frac{\\hat{\\sigma}^2}{\\sigma^2}\\right) + S_{\\mu}^2 \\left(\\frac{1}{\\hat{\\sigma}^2}- \\frac{1}{\\sigma^2}\\right)\\right]}$$")
    
  }else{
    texto2 <- paste("$$r(\\sigma^2)=\\frac{n}{2} \\left[ Ln \\left( \\frac{\\hat{\\sigma}^2}{\\sigma^2}\\right)-\\frac{\\hat{\\sigma}^2}{\\sigma^2} +1\\right] \\\\ 
                    R(\\sigma^2)=e^{\\frac{n}{2} \\left[ Ln \\left( \\frac{\\hat{\\sigma}^2}{\\sigma^2}\\right)-\\frac{\\hat{\\sigma}^2}{\\sigma^2} +1\\right]}$$")
  }
  
  
  return(list(texto, texto2))
  
 
  
}



